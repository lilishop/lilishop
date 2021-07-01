package cn.lili.common.validation.impl;

import cn.lili.common.validation.Mobile;
import cn.lili.common.validation.Phone;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class MobileValidator implements ConstraintValidator<Mobile, String> {

    @Override
    public boolean isValid(String value, ConstraintValidatorContext constraintValidatorContext) {
        Pattern p1 = null,p2 = null;
        Matcher m = null;
        boolean b = false;
        p1 = Pattern.compile("^[0][1-9]{2,3}-[0-9]{5,10}$");  // 验证带区号的
        p2 = Pattern.compile("^[1-9]{1}[0-9]{5,8}$");         // 验证没有区号的
        if(value.length() >9)
        {   m = p1.matcher(value);
            b = m.matches();
        }else{
            m = p2.matcher(value);
            b = m.matches();
        }
        return b;
    }

    @Override
    public void initialize(Mobile constraintAnnotation) {

    }
}
