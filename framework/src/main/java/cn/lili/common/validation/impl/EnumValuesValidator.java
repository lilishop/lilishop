package cn.lili.common.validation.impl;

import cn.lili.common.validation.EnumValue;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

/**
 * 枚举之校验
 *
 * @author Bulbasaur
 * @since 2021/7/9 1:41 上午
 */
public class EnumValuesValidator implements ConstraintValidator<EnumValue, Object> {

    private String[] strValues;
    private int[] intValues;

    @Override
    public boolean isValid(Object o, ConstraintValidatorContext constraintValidatorContext) {
        if (o instanceof String) {
            for (String s : strValues) {
                if (s.equals(o)) {
                    return true;
                }
            }
        } else if (o instanceof Integer) {
            for (int s : intValues) {
                if (s == ((Integer) o).intValue()) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public void initialize(EnumValue constraintAnnotation) {
        strValues = constraintAnnotation.strValues();
        intValues = constraintAnnotation.intValues();
    }
}
