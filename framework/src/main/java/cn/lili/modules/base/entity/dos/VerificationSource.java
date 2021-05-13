package cn.lili.modules.base.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 验证码资源维护
 * @author Chopper
 * @date 2021/1/30 4:13 下午
 */
@Data
@Entity
@Table(name = "li_verification_source")
@TableName("li_verification_source")
@ApiModel(value = "验证码资源维护")
public class VerificationSource extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "名称")
    private String name;

    @ApiModelProperty(value = "资源地址")
    private String resource;

    /**
     * @see cn.lili.modules.base.entity.enums.VerificationSourceEnum
     */
    @ApiModelProperty(value = "验证码资源类型 SLIDER/SOURCE")
    private String type;
}