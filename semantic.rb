def semantic(production)
    number=$NUM.dup
    literal=$LIT.dup
    identifier=$ID.dup
    for x in production
        case x
        when "NAME-> main "
            c=0;
            inter_code=Array.new;                                                   
            name='main';
            ids_lexeme=Array.new;
            ids_lexeme1=Array.new;
            datatype_stack=Array.new;
            datatype_stack_dup=Array.new;
            temp_count=0;
        when "FNAME-> NAME ( ) "
            fname=name+'()';
        when "DATATYPE-> char "
            datatype='%c';
        when "DATATYPE-> void "
            datatype='void';
        when "DATATYPE-> int "
            datatype='%d';
        when "IDS-> id "
            ids_object=identifier.shift;
            ids_object.inc_count; 
            datatype_stack_dup.push(ids_object);
            if datatype
                ids_type=datatype;
            else
                c=c+1;
            end
            ids_lexeme.push(ids_object.lex_value);
        when "IDS-> ID , IDS "
            ids_object=datatype_stack.pop();                                        
            if datatype
                ids_type=datatype;
            else
                c=c+1;
            end
            ids_lexeme.push(ids_object.lex_value);
        when "STMT-> DATATYPE IDS "
            for lx  in ids_lexeme
             if !ids_lexeme1.include?(lx)
               if h=$ID.find_all{|h| h.lex_value==lx}
                  for i in h
                    i.type_assign(datatype);
                  end
               end
             else
                 if h=identifier.find_all{|h| h.lex_value==lx}
            x      for i in h
                    i.type_assign(datatype);
                  end
                 end
                print 'ERROR:double decleration on line ';
                print line(lx,identifier);
                print "\n";
             end
            end
            ids_lexeme1=ids_lexeme1+ids_lexeme;
            ids_lexeme=[];
            datatype=nil;
        when "ID-> id "
            id_object=identifier.shift;
            id_object.inc_count;                                                   
            datatype_stack.push(id_object);
            datatype_stack_dup.push(id_object);
        when "FACTOR-> num "
            num_object=number.shift;
            fact_type=num_object.type_value;
            fact_value=num_object.value;
            fact_lexeme=''
            fact_line=num_object.line_value;
        when "FACTOR-> id "
            ids_object=identifier.shift;
            ids_object.inc_count;                                                   
            fact_type=ids_object.type_value;
            fact_lexeme=ids_object.lex_value;
            fact_value=ids_object.value
            fact_line=ids_object.line_value;
        when "TERM-> FACTOR "
            term_type=fact_type;
            term_lexeme=fact_lexeme
            term_value=fact_value;
            term_line=fact_line;
        when "EXPR-> TERM "
            expr_type=term_type;
            expr_lexeme=term_lexeme
            expr_value=term_value;
            expr_line=term_line;
        when "EXPR-> EXPR + TERM "
            #puts expr_type;
            #puts term_type;
            if expr_type!=term_type && expr_type !=''&&term_type!=''
             print 'ERROR:type missmatch on line ';
             print line(expr_lexeme,identifier);
             print "\n";
            end
            if x=$ID.find{ |x| x.lex_value==expr_lexeme}
                if x.value.nil?
                    print "ERROR:Variable #{expr_lexeme} not initialized\n"
                end
            end
            if x=$ID.find{ |x| x.lex_value==term_lexeme }
                if x.value.nil?
                    print "ERROR:Variable #{expr_lexeme} not initialized\n"
                end
            end
            if expr_type==''
                print 'ERROR: ';
                print expr_lexeme;
                print ' not declered on line ';
                print line(expr_lexeme,identifier);
                print "\n";
            end
            if term_type==''
                print 'ERROR: ';
                print term_lexeme;
                print ' not declered on line ';
                print line(term_lexeme,identifier);
                print "\n";
            end
            
        when "EXPR-> EXPR - TERM "
            if expr_type!=term_type && expr_type !=''&&term_type!=''
             print 'ERROR:type missmatch on line ';
             print line(expr_lexeme,identifier);
             print "\n";
            end
            if expr_type==''
                print 'ERROR: ';
                print expr_lexeme;
                print ' not declered on line ';
                print line(expr_lexeme,identifier);
                print "\n";
            end
            if term_type==''
                print 'ERROR: ';
                print term_lexeme;
                print ' not declared on line ';
                print line(term_lexeme,identifier);
                print "\n";
            end
            
        when "TERM-> TERM * FACTOR "
             if x=$ID.find_all{|x| x.lex_value==term_lexeme||x.lex_value==fact_lexeme}
                for i in x.uniq
                if i.type_value=="%c"
                   print "ERROR:character to interger conversion on line #{line(i.lex_value,identifier)}\n"
                elsif i.type_value=="%f"
                   i.type_assign("%f")
                else
                    i.type_assign("%d")
                end
               end
             end
         when "EXPR-> EXPR / TERM "
            if expr_type=="%d"&&term_type=="%d"
                expr_type="%f"
            elsif expr_type=="%f"||term_type=="%f"
                expr_type="%f"
            else expr_type=="%c"||term_type=="%c"
                print "ERROR:character devision on line #{line(expr_lexeme,identifier)}\n"
            end     
        when "STMT-> ID = EXPR "
            id_object=datatype_stack.pop;
            id_type=id_object.type_value;
            id_lexeme=id_object.lex_value;
            id_line=id_object.line_value;
            
            if id_type!=expr_type && id_type!=''&& expr_type!=''
                print 'ERROR: type mismatch on line ';
                print line(id_lexeme,identifier);
                print "\n";
            elsif id_type==''
                print 'ERROR: ';
                print ' not declared on line';
                print line(id_lexeme,identifier);
                print  "\n";
            elsif  expr_type==''
                print 'ERROR: ';
                print expr_value;
                print ' not declared ';
                print line(expr_value,identifier);
                print "\n";
            end
                id_object.value_assign(expr_value)
            
        when "STMT-> printf ( string , IDS ) "
            string=literal.shift;
            count_ids=(string.lit_value).scan(/%[dfcu]/).count;
            prnt_object=Array.new;
            if count_ids!=c
                 print 'ERROR:parameter number mismatched on line ';
                 print  string.line_value;
                 print  'for printf(...)'
                 print "\n";
            else
                i=0;
                while i<count_ids
                  prnt_object[i]=datatype_stack_dup.pop;
                  i=i+1;
                end
                
                new_string=(string.lit_value).scan(/%[dfuc]/);
                for i in new_string
                   new_prnt_object=prnt_object.pop;
                   if new_prnt_object.type_value!=i
                       print 'ERROR:type mismatch in printf() for lexeme:';
                       print new_prnt_object.lex_value;
                       print "\n";
                   end 
                end
            end
        end
    end
    #print "#{prefernece('%d','%d')}\n"
    return inter_code    
end
def line(lexeme,identifier)
    if k=$ID.find{|k| k.lex_value==lexeme}
    	cntr=k.counter_value;
    	lines=k.line_value;
        line_array=lines.split('/');
    end
    	return line_array[cntr-1];
end
def prefernece(type1,type2)
    if type1=="%d"&&type2=="%d"
        new_type="%d"
    elsif type1=="%f"&&type2=="%d"
        new_type="%f"
    elsif type1=="%d"&&type2=="%f"
        new_type="%f"
    elsif type1=="%f"&&type2=="%f"
        new_type="%f"
    elsif type1=="%c"||type2=="%c"
        new_type="err"
    end
    return new_type
end