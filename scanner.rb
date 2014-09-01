class Identifier
  @lexeme=""
  @type=""
  @line=""
  @counter=0
  @value=nil
  def initialize(lexeme,line)
    @lexeme=lexeme
    @line="#{line}/"
    @type=""
    @counter=0;
    @value=""
  end
  def pos(line)
    @line=@line+line.to_s+"/"
  end
  def lex_value
    @lexeme
  end
  def type_assign(type)
    @type=type
  end
  def type_value
    @type
  end
  def inc_count
    @counter=@counter+1
  end
  def line_value
    @line
  end
  def counter_value
    @counter
  end
  def value_assign(value)
    @value=value
  end
  def value
    @value
  end

end

class Number
  @value=nil
  @type=""
  @line=nil
  def initialize(value,type,line)
    @value=value
    @type=type
    @line=line
  end
  def num_value
    @value
  end
  def type_value
    @type
  end
  def line_value 
    @line
  end
  def value
    @value
  end
end

class Literal
  @value=nil
  @line=nil
  def initialize(value,line)
    @value=value
    @line=line
  end
  def lit_value
    @value
  end
  def line_value
    @line
  end
end
def scanner(words,file)
  $ID=[];
  $NUM=[];
  $LIT=[];
  delim=0                                                                 #
  paran=0 
  e=0  
  #p words
  #-------------------------------------Scanning Phase-------------------------------------------------------------------------------------
  
  lexem_id={};
  words.map! do|word|
    if word.include?("\"")
      i=0
      f=0
      temp=[]
      i=0;
      while i<word.length
        if f==0
          unless (word[i] == " ")and(word[i+1]==" ")
            temp<<word[i]
          end
          if word[i]=="\""
            f=1
          end
        else
          temp<<word[i]
          if word[i]=="\""
            unless word[i-1]=="\\"
              f=0
            end
          end
        end
        i=i+1
      end
      word=temp.join()
    else
      word=word.gsub(/[\s]+/," ")
    end 
  end

  #p words
  line=1  
 #---------------------------------------LexicalAnalysis Phase---------------------------------------------------------------------------  
  for word in words
    i=0
    while i<word.length
      peek=word[i]
      unless peek == " " or peek == "\t"  
  #<!---------------- Check for digit ---------------------------!>      

        if !(peek=~/^[0-9]$/).nil?
          v=0
          begin
            v=v*10 + peek.to_i
            i=i+1
            peek=word[i]
          end while !(peek=~/^[0-9]$/).nil?
          if peek == '.'                                                            #
            x=v
            d=10
            while(1)
              i=i+1
              peek=word[i]
              if (peek=~/^[0-9]$/).nil?
                break
              end
              x=x+(peek.to_f/d);
              d=d*10
            end
            ob=Number.new(x,"%f",line)
          else
            ob=Number.new(v,"%d",line)          
          end
          #file.puts "token <num,#{ob}>"
          file.puts "num"
          $NUM<<ob
          next
        #end
  #<!---------------- Check for Identifier/Keyword ------------------------------!> 

        elsif !(peek=~/^[A-Za-z]$/).nil?
          b=""
          begin
            b=b+peek
            i=i+1
            peek=word[i]
          end while !(peek=~/^[A-Za-z0-9]$/).nil? 
            keyword=["int","float","char","double","long","short","signed","unsigned","void","main","printf"];
          if keyword.include?(b)
            #file.puts "token <#{b}>"
            #indent=b
            file.puts "#{b}"
          else
            if lexem_id[b].nil?
              ob=Identifier.new(b,line)
              lexem_id[b]=ob
            else
              ob=lexem_id[b]
              ob.pos(line)
             end 
            $ID<<ob
            #file.puts "token <id,#{ob}>"
            file.puts "id"
          end
          next
        #end
  #<!---------------- Check for Operator ------------------------------------!>

        elsif !(peek =~ /^[()+-;.=,\*\/\[\]{}%]$/).nil?
          if peek=="{"                                                    #
            delim=delim+1
          elsif peek=="}"
            delim=delim-1
          elsif peek=="("
            paran=paran+1
          elsif peek==")"
            paran=paran-1
          end
          #file.puts "token <#{peek}>"
          file.puts "#{peek}"
        #end
        
  #<!---------------- String Literal -------------------------------------------!>

        elsif peek == "\""
          m=word.count "\""                                            #
          n=word.scan(/\\\"/).count                                    #
          if (m-n)%2 !=0                                               #
            puts "Lexical Error:Line number #{line}: expected \" "     #
            e=1                                                        #  
          end                                                          #
          b=""
          while(e!=1)
            b=b+peek
            i=i+1
            peek=word[i]
            if peek =="\""
              unless word[i-1]=="\\"
                b=b+peek
                break
              end
            end
          end
          ob=Literal.new(b,line)
          #file.puts "token <lit,#{ob}>"
          file.puts "string"
          $LIT<<ob
          i=i+1
          next
        #end
  #<!----------------Panic Mode ------------------------------------------------!>

        else
          i=i+1
        end  


      end
      i=i+1
    end
    line=line+1
  end
  if delim != 0                                                                         #
    puts "Lexical Error: Line number #{line} :Unmatched Delimiters" ;
    e=1
  end
  if paran != 0
    puts "Lexical Error: Line number #{line} :Unmatched Paranthesis" ;
    e=1
  end
  if e==1
    exit
  end
end