#!/usr/bin/ruby

require 'rubygems'
require_gem 'happymapper'
require 'happymapper'
require 'pp'

filename = ARGV[0]

$current_year = 2009
$months = ["","Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"]

# all nodes in a level will not have parent. only the first node will have the parent
class Nary
  attr_accessor :val
  attr_accessor :parent
  attr_accessor :next
  attr_accessor :prev
  attr_accessor :child
  
  def initialize(v=nil)
    @val = v
    @parent = nil
    @next = nil
    @prev = nil
    @child = nil
    v.node = self
  end
  
  def equal(o={ })
    o.each_pair { |k,v|
      #puts "key #{k} val #{v} : incomp #{val.send(k)} equal #{val.send(k) != v}"
      return false if val.send(k) != v
    }
    return true
  end
  
  def is_first
    return @prev ? false : true
  end
  
  def get_first()
    first = self
    while first.is_first
      first = first.prev
    end
    return first
  end
  
  def get_last
    last = self
    while last.next
      last = last.next
    end
    return last
  end

  # adds the node to the next of the first child
  def add_child(node)
    return if ! node
    if @child
      @child.add_sibling(node)
    else
      node.detach
      @child = node
    end
  end
  
  def add_sibling(node)
    return if ! node
    node.detach
    i_last = node.get_last()
    @next.prev = i_last if @next
    i_last.next = @next
    @next = node
    node.prev = self
  end
  
  def detach
    #detach from prev
    @prev.next = nil if @prev
    @prev = nil
    
    #detach from parent
    @parent.child = nil if is_first() and @parent
  end
  
  def attach(node)
    node.add_child(self)
  end
  
  def self.find_depth_first(node = nil, o={ })
    return nil if ! node
    return node if node.equal(o)
    in_children = find_depth_first(node.child, o)
    return in_children if in_children
    in_next = find_depth_first(node.next, o)
    return in_next 
  end
  
  #only give root node as argument
  def self.traverse_and_print_as_xml(node=nil)
    return "" if ! node
    str = "<node><value>#{node.val.id}-#{node.val.name}</value>"
    str += "<child>#{traverse_and_print_as_xml(node.child)}</child>" if node.child
    str += "</node>"
    str += traverse_and_print_as_xml(node.next)
    return str
  end
end

class Account
  attr_accessor :id
  attr_accessor :type
  attr_accessor :name
  attr_accessor :node
  def initialize(id, name, type)
    @id = id
    @name = name
    @type = type
    @node = nil
  end
end

class Accounts
  attr_accessor :tree
  def initialize
    @tree = nil
  end
  def add_account(ac, parent=nil)
    ac = Nary.new(ac) if ac and !ac.is_a?(Nary) and ac.is_a?(Account)
    if !parent 
      if ! @tree
        @tree = ac if ! @tree
        return true
      end
      @tree.add_sibling(ac)
      return true
    end
    if parent.is_a?(Nary)
      parent.add_child(ac)
      return true
    end
    pnode = Nary.find_depth_first(@tree, parent)
    raise "parent not found" if !pnode
    pnode.add_child(ac)
    return true
  end
  def find(o={ })
    return nil if ! @tree
    node = Nary.find_depth_first(@tree, o)
    return nil if ! node
    return node.val
  end
  
  def self.read_from_file(filename)
    acs = Accounts.new
    xml = File.read(filename)
    doc = xml.to_libxml_doc
    root = doc.root
    accounts = root.find(".//gnc:account")
    accounts.each { |ac|
      id = ac.find_first(".//act:id[@type='guid']").content
      name = ac.find_first(".//act:name").content
      type = ac.find_first(".//act:type").content
      parent = ac.find_first(".//act:parent")
      parent_id = parent ? { :id => parent.content } : nil
      acs.add_account(Account.new(id, name, type), parent_id)
    }
    return acs
  end
end



class Budget
  attr_accessor :id
  attr_accessor :name
  attr_accessor :period
  attr_accessor :accounts
  attr_accessor :rows

  def initialize(id, name, period)
    @id = id
    @name = name
    @period = period
    @accounts = []
    @rows = []
  end
  
  def add_account(account, budget_row)
    @accounts << account
    @rows << budget_row
  end
  
  def sum
    return rows.inject(0) { |sum, row| 
      sum + row.inject(0) { |s0, n| s0 + n}
    }
  end
  
  def select(type)
    rows = []
    accounts.each_index { |i|
      ac = accounts[i]
      #puts ac.type
      if ac.type == type
        rows << @rows[i]
      end
    }
    #pp rows
    return rows
  end

  def on_month(month)
    c = []
    @rows.each_index { |i|
      c << { :account => @accounts[i], :budget => (@rows[i][month]?@rows[i][month]:0)}
    }
    return c
  end
  
  def sum_rows(type)
    return select(type).inject(0) { |sum, row| 
      sum + row.inject(0) { |s0, n| s0 + n}
    }
  end
  
  def expenses
    return sum_rows("EXPENSE")
  end
  
  def incomes
    return sum_rows("INCOME")
  end
  
  def liabilities
    return sum_rows("LIABILITY")
  end
  
  def print_account_wise
    income = incomes
    merged = accounts.zip(@rows)
    
    rep = Proc.new { |type, accounts|
      exp = accounts.reject { |m| m[0].type != type}
      exp.each { |a|
        sum = a[1].inject(0) { |s,e| s+e}
        printf "%-50.50s                     : %00005.2f %s %0.2f %s \n" % [a[0].name, sum, "(", sum * 1.0/income *100, "%%)" ]
      }
    }
    
    puts "Expenses:"
    rep.call("EXPENSE", merged)
    
    puts "\nIncome"
    rep.call("INCOME", merged)
    
    puts "\nLiability"
    rep.call("LIABILITY", merged)
    
    puts
  end
  
  def print
    @rows.each { |r|
      pp r
    }
  end
  
  def self.read_from_file(filename, acs)
    xml = File.read(filename)
    doc = xml.to_libxml_doc
    root = doc.root
    ac = root.find_first(".//gnc:budget")
    id = ac.find_first(".//bgt:id[@type='guid']").content
    name = ac.find_first(".//bgt:name").content
    periods = ac.find_first(".//bgt:num-periods").content.to_i
    bgt = Budget.new(id, name, periods)
    
    slots = ac.find("./bgt:slots/slot")
    slots.each { |slot|
      key = slot.find_first(".//slot:key").content
      account = acs.find({ :id => key})
      row = Array.new(periods, 0)
      subslots = slot.find(".//slot")
      subslots.each { |sub|
        key = sub.find_first(".//slot:key").content
        value = eval(sub.find_first(".//slot:value").content)
        #places = value.split("/")
        #value = value.split("/")[0].to_i
        #value = value / places[1].to_i if places.length > 1
        row[key.to_i] = value
      }
      bgt.add_account(account, row)
    }
    
    return bgt

  end
  
end


# construct object model from file
#acs = Accounts.new

# simple test
#acs.add_account(Account.new("1", "a", "EXPENSE" ))
#acs.add_account(Account.new("5", "e", "EXPENSE" ))
#acs.add_account(Account.new("2", "b", "EXPENSE" ), { :id => "1"})
#acs.add_account(Account.new("4", "d", "EXPENSE" ), { :id => "1"})
#acs.add_account(Account.new("3", "c", "EXPENSE" ), { :id => "4"})
#pp acs.find({ :id => "1"})

class Command
  def execute(bgt)
  end
end

class SummaryCommand < Command

  def execute(bgt)
    bgt_expenses = bgt.expenses
    bgt_incomes  = bgt.incomes
    bgt_liabilities = bgt.liabilities
    bgt_surplus = bgt_incomes - bgt_expenses - bgt_liabilities

    bgt_income_percentage = (bgt_expenses * 1.0/ bgt_incomes) * 100.0
    bgt_surplus_percentage = (bgt_surplus * 1.0 / bgt_incomes) * 100.0

    bgt.print_account_wise

    printf "Budget Expense           : %00.2f  %00.2f%%\n" % [bgt_expenses,  bgt_income_percentage]
    printf "Budget Incomes           : %00.2f\n" % bgt_incomes
    printf "Budget Liablities        : %00.2f\n\n" % bgt_liabilities


    printf "Surplus                  : %00.2f  %0.2f%%\n" % [bgt_surplus, bgt_surplus_percentage]
  end
end

class BudgetCommand
  def execute(bgt)
    pp ["month", "expenses", "income", "liablity", "saving"]
    (0..11).each { |r|
      old = [0,0,0,0,0] # month, expenses, income, liability, saving
      on_month = bgt.on_month(r)
      expenses = on_month.select { |m| m[:account].type == "EXPENSE"}.inject(0) { |s, m| s + m[:budget]}
      income = on_month.select { |m| m[:account].type == "INCOME"}.inject(0) { |s, m| s + m[:budget]}
      liability = on_month.select { |m| m[:account].type == "LIABILITY"}.inject(0) { |s, m| s + m[:budget]}
      old = ["#{$months[r+1]} #{$current_year + ((r+1+2)/12)}", old[1] + expenses, old[2] + income, old[3] + liability, old[4] + income - (expenses + liability)]
      pp old
    }

    for_month = ARGV[2].to_i
    r = for_month
    pp "For month #{$months[r+1]} #{$current_year + ((r+1+2)/12)}:"
    on_month = bgt.on_month(r)
    on_month.each { |row|
      puts "%100s %20s %20s %20s" % [row[:account].name, row[:account].type, row[:budget], row[:account].node == nil]
    }
  end
end


class ProjectionCommand
  def execute(bgt)
    pp ["month", "expenses", "income", "liablity", "saving"]
    old = [0,0,0,0,0] # month, expenses, income, liability, saving
    (0..11).each { |r|
      on_month = bgt.on_month(r)
      expenses = on_month.select { |m| m[:account].type == "EXPENSE"}.inject(0) { |s, m| s + m[:budget]}
      income = on_month.select { |m| m[:account].type == "INCOME"}.inject(0) { |s, m| s + m[:budget]}
      liability = on_month.select { |m| m[:account].type == "LIABILITY"}.inject(0) { |s, m| s + m[:budget]}
      old = ["#{$months[r+1]} #{$current_year + ((r+1+2)/12)}", old[1] + expenses, old[2] + income, old[3] + liability, old[4] + income - (expenses + liability)]
      pp old
    }
  end
end

class CommandFactory
  def self.getCommand(cmd)
    command =  case cmd
               when "summary" then SummaryCommand.new
               when "projection" then ProjectionCommand.new
               when "budget" then BudgetCommand.new
               else SummaryCommand.new
               end
    return command
  end
end


filename = ARGV[0]
command = ARGV[1]

acs = Accounts.read_from_file(filename)
#puts "<root>"
#puts Nary.traverse_and_print_as_xml(acs.tree)
#puts "</root>"
bgt = Budget.read_from_file(filename, acs)
#bgt.print


CommandFactory.getCommand(command).execute(bgt)
