package cn.lili.modules.promotion.entity.dto;


import cn.lili.modules.promotion.entity.dos.KanjiaActivityLog;
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
public class KanjiaActivityDTO extends KanjiaActivityLog {

    @ApiModelProperty(value = "砍价商品Id")
    private String kanjiaActivityGoodsId;

}