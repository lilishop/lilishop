package cn.lili.modules.page.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 专题活动
 *
 * @author Bulbasaur
 * @date 2020/12/10 17:42
 */
@Data
@Entity
@Table(name = "li_special")
@TableName("li_special")
@ApiModel(value = "专题活动")
public class Special extends BaseEntity {

    @ApiModelProperty(value = "专题活动名称")
    private String specialName;

    /**
     * @see cn.lili.modules.base.entity.enums.ClientTypeEnum
     */
    @ApiModelProperty(value = "楼层对应连接端类型", allowableValues = "PC,H5,WECHAT_MP,APP")
    private String clientType;

    @ApiModelProperty(value = "页面ID")
    private String pageDataId;
}
