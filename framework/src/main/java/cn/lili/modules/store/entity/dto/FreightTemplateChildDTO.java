package cn.lili.modules.store.entity.dto;

import cn.lili.modules.store.entity.dos.FreightTemplateChild;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.beans.BeanUtils;

import javax.validation.constraints.NotEmpty;


/**
 * 模版详细配置
 *
 * @author pikachu
 * @since 2018-08-22 15:10:51
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class FreightTemplateChildDTO extends FreightTemplateChild {

    private static final long serialVersionUID = -4143478496868965214L;


    /**
     * @see cn.lili.modules.store.entity.enums.FreightTemplateEnum
     */
    @NotEmpty(message = "计价方式不能为空")
    @ApiModelProperty(value = "计价方式：按件、按重量", allowableValues = "WEIGHT, NUM")
    private String pricingMethod;

    public FreightTemplateChildDTO(FreightTemplateChild freightTemplateChild) {
        BeanUtils.copyProperties(freightTemplateChild, this);
    }
}