package cn.lili.common.validation;

import cn.lili.common.validation.impl.PhoneValidator;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * 手机号码校验注解
 *
 * @author Bulbasaur
 */
@Target({METHOD, FIELD, ANNOTATION_TYPE, CONSTRUCTOR, PARAMETER})
@Retention(RUNTIME)
@Documented
@Constraint(validatedBy = {PhoneValidator.class})
public @interface Phone {

    String regexp() default "1[3|4|5|7|8]\\d{9}";

    String message() default "手机号码格式不正确";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
