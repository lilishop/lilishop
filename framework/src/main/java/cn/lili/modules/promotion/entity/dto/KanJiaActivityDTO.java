package cn.lili.modules.promotion.entity.dto;


import cn.lili.modules.promotion.entity.dos.KanJiaActivityLog;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * 砍价活动参与实体类
 *
 * @author qiuqiu
 * @date 2020-7-1 10:44 上午
 */
@Data
@ApiModel(value = "砍价活动参与记录对象")
public class KanJiaActivityDTO extends KanJiaActivityLog {

    @ApiModelProperty(value = "砍价金额")
    private String kanJiaActivityGoodsId;

}