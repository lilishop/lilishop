package cn.lili.modules.message.entity.dos;

import cn.lili.base.IdEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * @author Chopper
 */
@Data
@Entity
@Table(name = "li_short_link")
@TableName("li_short_link")
@ApiModel(value = "短链接/暂时只用于小程序二维码业务")
public class ShortLink extends IdEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "原始参数")
    private String originalParams;


}