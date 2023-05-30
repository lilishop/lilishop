package cn.lili.common.validation.impl;

import cn.lili.common.validation.Mobile;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 电话校验
 * 支持手机号+电话同时校验
 *
 * @author Bulbasaur
 * @since 2021/7/9 1:41 上午
 */
public class MobileValidator implements ConstraintValidator<Mobile, String> {

    private static final Pattern PHONE = Pattern.compile("^0?(13[0-9]|14[0-9]|15[0-9]|16[0-9]|17[0-9]|18[0-9]|19[0-9])[0-9]{8}$");
    private static final Pattern AREA_MOBILE = Pattern.compile("0\\d{2,3}[-]?\\d{7,8}|0\\d{2,3}\\s?\\d{7,8}|13[0-9]\\d{8}|15[1089]\\d{8}");
    private static final Pattern MOBILE = Pattern.compile("^[1-9]{1}[0-9]{5,8}$");

    @Override
    public boolean isValid(String value, ConstraintValidatorContext constraintValidatorContext) {
        Matcher m = null;
        // 验证手机号
        if (value.length() == 11) {
            m = PHONE.matcher(value);
            // 验证带区号的电话
        } else if (value.length() > 9) {
            m = AREA_MOBILE.matcher(value);
            //验证没有区号的电话
        } else {
            m = MOBILE.matcher(value);
        }
        return m.matches();
    }


    @Override
    public void initialize(Mobile constraintAnnotation) {

    }
}
