def closure(i,g)
  j=i
  f=0
  begin
    f=0
    for k in j.find_all{|item| item =~ /^[A-Z]+->[ ][\W0-9a-zA-Z ]*[.][ ][A-Z]+[ ]?[a-zA-Z0-9\W ]*$/}
      l=k[/[.][ ][A-Z]*[ ]/]
      l.delete!(". ")
      for m in g.find_all{|item| item =~ /^#{l}->/}
        n=m[/->[ ][\W0-9A-Za-z ]+/]
        n.gsub!(/->/, '')
        n=l+"-> ."+n+" "
        n=n.gsub(/[\s]+$/," ")
        unless j.include?(n)
          j<<n
          f=1
        end
      end
    end 
  end while(f==1)
  return j  
end

def got(i,x,g)
    l=[ ]
    y=x
    x=x.split("").join("[")
    x=x.insert(0,"[")
    x=x.scan(/./).each_slice(2).map(&:join).join("]")
    x=x.insert(x.length,"]")
    for k in i.find_all{|item| item =~ /^[A-Z]+->[ ][\W0-9a-zA-Z ]*[.] #{x} [\Wa-zA-Z0-9 ]*$/}
    a=k[/^[A-Z]+->[ ][\WA-Za-z ]*[.]/]
    a=a.gsub(".","")
    b=k[/[.] #{x} [\Wa-zA-Z ]*$/]
    #b=k[/[.] #{x} /]
    #c=k[/[.] #{x} [\Wa-zA-Z0-9 ]*$/]
    #c=c.gsub(b,"  ")
    #b=b.gsub(". "," ")
    b=b.gsub(". #{y} "," ")
    a=a+y+" ."+b
    a=a.gsub(/[\s]+$/," ")
    l<<a
  end   
    return closure(l,g)
end

def items(g,gram_sym, start_sym)
  c=[closure(["SS-> . #{start_sym} "],g)]
  begin
    f=0
    for i in c
      for x in gram_sym
        h=got(i,x,g)
        unless h.empty?
          unless c.include?(h)
            c<<h
            f=1
          end 
        end
      end
    end
  end while(f==1)
  return c
end



def firstof(x,term_sym, non_term_sym,g)
  f=[ ]
  if term_sym.include?(x)
    f=[x]
  elsif non_term_sym.include?(x)
    for k in g.find_all{|item| item =~ /^#{x}->/}
      n=nil
      l=k[/->[\Wa-zA-Z0-9 ]+/]
      l.delete!("->")
      l=l.split(" ")
      for m in l
        unless firstof(m, term_sym, non_term_sym, g).include?("")
          n=m
          break;
        end
      end
        if !(n.nil?)
        p=firstof(n, term_sym, non_term_sym, g) 
        for q in p
          unless f.include?(q)
            f<<q
          end
        end
      else
          f<<"" if !(f.include?(""))
      end
    end
  end
  return f
end

def followof(x, term_sym, non_term_sym, g)
  f=[ ]
  y=x
  if y=="S"
    f<<"$" if !(f.include?("$"))
  end
    x=x.split("").join("[")
    x=x.insert(0,"[")
    x=x.scan(/./).each_slice(2).map(&:join).join("]")
    x=x.insert(x.length,"]")
    for k in g.find_all{|item| item =~ /^[A-Z]+->[ ][\W0-9a-zA-Z ]*#{x} [\Wa-zA-Z0-9]+[ ][\Wa-zA-Z0-9 ]*$/}
      l=k.gsub(/^[A-Z]+->[ ]/,"")
      l=l[/#{x}[ ][^\s]+[ ]/]
      l=l.gsub("#{y}", "")
      l=l.gsub(/^[\s]+/,"")
      l=l.gsub(/[\s]+$/,"")
      a=k[/^[A-Z]+->/]
      a=a.gsub("->","")
      #unless a==y
        n=firstof(l, term_sym, non_term_sym, g)
        for m in n  
          unless m=="epsilon"
            f<<m if !(f.include?(m))
          end
        end
        if n.include?("epsilon")
          for b in followof(a,term_sym,non_term_sym,g)
            f<<b if !(f.include?(b))
          end
        end
      #end 
    end
    for k in g.find_all{|item| item =~ /^[A-Z]+->[ ][\Wa-zA-Z0-9]* #{x} $/}
      a=k[/^[A-Z]+->/]
      a=a.gsub("->","") 
      unless a==y
        for b in followof(a,term_sym,non_term_sym,g)
          f<<b if !(f.include?(b))
        end
      end
    end
return f
end



def table(g, table_action_sym, table_goto_sym, gram_sym, start_sym)
  c=items(g,gram_sym, start_sym)
  $action=Array.new(c.length){Array.new(table_action_sym.length)}
  $goto_table=Array.new(c.length){Array.new(table_goto_sym.length)}
  for i in (0...c.length)
    for k in c[i].find_all{|item| item =~ /^[A-Z]+->[ ][\W0-9a-zA-Z ]*[.] [^\sA-Z]+ [\Wa-zA-Z0-9 ]*$/}
      a=k[/[.] [^\sA-Z]+[ ]/]
      a=a.gsub(". ","")
      a=a.gsub(" ","")
      d=got(c[i],a,g)
      j=c.index(d)
      if table_action_sym.index(a).nil?
        puts "unknown identifier"
        return
      end
      unless j.nil?
        $action[i][table_action_sym.index(a)]="s#{j}"
      end
    end

    for k in c[i].find_all{|item| item =~ /^[A-Z]+->[ ][\W0-9a-zA-Z ]*[.][ ]$/}
      #puts k
      l=k.gsub(". ","")
      a=k[/^[A-Z]+->/]
      a=a.gsub("->","")
      #unless a == "SS"
        j=g.index(l)
        #puts j
        for m in followof(a, table_action_sym, table_goto_sym,g)
          unless table_action_sym.index(m).nil?
            $action[i][table_action_sym.index(m)]="r#{j+1}"
          end
        end
      #end
    end 

    if c[i].include?("SS-> #{start_sym} . ")
      $action[i][table_action_sym.index("$")]="ac"
    end

    for k in table_goto_sym
      d=got(c[i],k,g)
      j=c.index(d)
      unless j.nil?
        $goto_table[i][table_goto_sym.index(k)]=j
      end
    end
  end

  #for i in (0...c.length)
  # for j in (0...table_action_sym.length)
  #   print $action[i][j]
  #   print "\t"
  # end
  #
  # for j in (0...table_goto_sym.length)
  #   print $goto_table[i][j]
  #   print "\t"
  # end
  # print("\n")
  #end

  #print "\n\n\n"

end

def parse(word,term_sym,non_term_sym,g)
  prod=[]
  word=word+"$"
  words=word.split(/[\s]/)
  len=words.length
  j=0
  st=[0]
  l=0
    while true
      a=words[j]
    s=st[l]
    unless s.nil?
    d=$action[s.to_i][term_sym.index(a)]
    end
    #for z in (0..l)
    # print st[z]
    #end
    #print "\t\t"
    #print d
    #print "\t\t\n"
    if d =~ /^[s]/
      b=d.gsub(/^[s]/,"")
      l=l+1
      st[l]=b.to_i
      j=j+1
    elsif d =~ /^[r]/
      b=d.gsub(/^[r]/,"")
      b=b.to_i
      x=g[b-1]
      c=x[/^[A-Z]+->/]
      y=x[/->[\Wa-zA-Z0-9 ]+$/]
      y=y.gsub("->","")
      c=c.gsub("->","")
      y=y.split(" ")      
      for i in (0...y.length)
        l=l-1
      end
      t=st[l]
      l=l+1
      st[l]=$goto_table[t.to_i][non_term_sym.index(c)]
      #puts x
      prod<<x
    elsif d=="ac"
      puts "ACCEPTED"
      return prod
    else  
      puts "ERROR"
      break 
    end
  end
  
end



def is_up?(word)
    !(word =~ /^[A-Z]*$/).nil?
end

def parse_tree(input_dup)
  tree=Array.new
  stack=Array.new
  val=""
  id_finder=0
  for i in 0..(input_dup.length-1)
    inputvalue=input_dup[i]
    dup_array=inputvalue.split("->")
    id_finder=id_finder+1
    value={:id=>id_finder,:data=>dup_array[0],:parent_id=>nil}
    dup_inputvalue=dup_array[1]
    j=dup_inputvalue.length-2
    while j>=0
      f=0
      while dup_inputvalue[j]!=" "
         val+=dup_inputvalue[j]
               j=j-1
         f=1
      end
          if f==1
        j=j-1
      end
      val=val.reverse
      if(!is_up?(val))  
        id_finder=id_finder+1
        tree.push(:id=>id_finder,:data=>val,:parent_id=>value[:id])   
      else 
         if h = stack.find { |h| h[:data]==val&&h[:parent_id]==nil}
                v = stack.pop
                if r=tree.find {|r| r==v }
                  tree.delete(r)
                  r[:parent_id]=value[:id]
                  tree.push(r)
                end
              else
                print "ERROR"
                exit
             end
        end
          val=""
    end
    stack.push(value)
    tree.push(value)
  end
  tree=tree.reverse
  root = {:id => 0, :data => '', :parent_id => nil}
  map = {}

  tree.each do |e|
    map[e[:id]] = e
  end

  @@tree = {}

  tree.each do |e|
    pid = e[:parent_id]
    if pid == nil || !map.has_key?(pid)
      (@@tree[root] ||= []) << e
    else
      (@@tree[map[pid]] ||= []) << e
    end
  end
  def print_tree(item, level)
    items = @@tree[item]
    unless items == nil
      indent = level > 0 ? sprintf("%#{level * 2}s", " ") : ""
      items.each do |e|
        puts "#{indent}-#{e[:data]}"
        print_tree(e, level + 1)
      end
    end
  end
  print_tree( root, 0)
  return tree
end







def parser(z)
  #g=[ "S-> T SS ", "SS-> + T SS ", "SS-> epsilon ", "T-> F TT ", "TT-> * F TT ", "TT-> epsilon ", "F-> ( S ) ", "F-> id "]
  #gram_sym=[ "S","SS","T","TT","F","+","*","id","(",")"]
  #term_sym=[ "id","+","*","(",")","$","epsilon"]
  #non_term_sym=["S","T","F","SS", "TT"]
  #print followof("SS",term_sym,non_term_sym,g)

  #g=[ "S-> L = R ","S-> R ","L-> * R ","L-> id ","R-> L "]
  #gram_sym=["S","L","R","=","*","id"]
  #for x in items(g,gram_sym)
  #print x
  #print "\n"
  #end
  #---------------------------------------A->B--&B->C---does'nt work-------------------------------------
  g=["S-> DATATYPE FNAME { STMTS } ",
     "STMTS-> STMT ; STMTS ",
     "STMTS-> STMT ; ",
     "STMT-> DATATYPE IDS ",
     "STMT-> ID = EXPR ",
     "STMT-> printf ( string , IDS ) ",
     "STMT-> printf ( string ) ",
     "EXPR-> EXPR + TERM ",
     "EXPR-> EXPR - TERM ",
     "EXPR-> TERM ",
     "EXPR-> EXPR / TERM ",
     "TERM-> TERM * FACTOR ",
     "TERM-> FACTOR ",
     "FACTOR-> ( EXPR ) ",
     "FACTOR-> id ",
     "FACTOR-> num ", 
     "IDS-> ID , IDS ",
     "IDS-> id ", 
     "DATATYPE-> int ",
     "DATATYPE-> void ",
     "DATATYPE-> char ",
     "FNAME-> NAME ( ) ",
     "NAME-> main ",
     "ID-> id "]
     $GRAM_SYM=g
  gram_sym=["S","ID","DATATYPE","EXPR","TERM","FACTOR", "FNAME","NAME","STMT", "IDS" ,"STMTS","id","num","int","char","void","main","printf","string","+","*","=","{","}","(",")",";",",","$","epsilon","-","/"]
  term_sym=["id","num","int","void","char","main","printf","string","+","*","=","{","}","(",")",";",",","$","epsilon","-","/"]
  non_term_sym=["S","DATATYPE","EXPR","TERM","FACTOR" ,"FNAME","NAME","STMT","IDS","STMTS","ID"]
  start_sym="S"
  
  #g=["S-> S + T ","S-> T ","T-> T * F ","T-> F ","F-> ( S ) ","F-> id "]
  #gram_sym=["S","T","F","id","+","*","(",")","$","epsilon"]
  #term_sym=["id","+","*","(",")","$","epsilon"]
  #non_term_sym=["S","T","F"]
  #start_sym="S"
  #c=[closure(["SS-> . #{start_sym} "],g)]
  #puts c
  #z=items(g,gram_sym,start_sym)
  #puts z.length
  #for x in z
  #print x
  #print "\n"
  #end
  #print followof("S",term_sym,non_term_sym,g)
  table(g,term_sym,non_term_sym,gram_sym, start_sym)
  return parse(z,term_sym,non_term_sym,g)
end