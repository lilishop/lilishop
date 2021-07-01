package cn.lili.common.validation.impl;

import cn.lili.common.validation.Mobile;
import cn.lili.common.validation.Phone;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 电话校验
 * 支持手机号+电话同时校验
 */
public class MobileValidator implements ConstraintValidator<Mobile, String> {

    @Override
    public boolean isValid(String value, ConstraintValidatorContext constraintValidatorContext) {
        Pattern p1 = null, p2 = null, p3 = null;
        Matcher m = null;
        p1 = Pattern.compile("0\\d{2,3}[-]?\\d{7,8}|0\\d{2,3}\\s?\\d{7,8}|13[0-9]\\d{8}|15[1089]\\d{8}");  // 验证带区号的
        p2 = Pattern.compile("^[1-9]{1}[0-9]{5,8}$");         // 验证没有区号的
        p3 = Pattern.compile("^0?(13[0-9]|14[0-9]|15[0-9]|16[0-9]|17[0-9]|18[0-9]|19[0-9])[0-9]{8}$");// 验证手机号
        if (value.length() == 11) {
            m = p3.matcher(value);
        } else if (value.length() > 9) {
            m = p1.matcher(value);
        } else {
            m = p2.matcher(value);
        }
        return m.matches();
    }


    @Override
    public void initialize(Mobile constraintAnnotation) {

    }
}
