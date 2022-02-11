package cn.lili.modules.goods.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;


/**
 * 直播间商品
 *
 * @author Bulbasaur
 * @since 2021/5/18 5:42 下午
 */
@Data
@ApiModel(value = "直播间商品")
@TableName("li_studio_commodity")
@NoArgsConstructor
public class StudioCommodity extends BaseIdEntity {

    @ApiModelProperty(value = "房间ID")
    private Integer roomId;

    @ApiModelProperty(value = "商品ID")
    private Integer goodsId;

    public StudioCommodity(Integer roomId, Integer goodsId) {
        this.roomId = roomId;
        this.goodsId = goodsId;
    }
}
