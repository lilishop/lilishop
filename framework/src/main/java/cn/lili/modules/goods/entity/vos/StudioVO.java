package cn.lili.modules.goods.entity.vos;

import cn.lili.modules.goods.entity.dos.Commodity;
import cn.lili.modules.goods.entity.dos.Studio;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * 直播间VO
 *
 * @author Bulbasaur
 * @since 2021/5/31 11:58 上午
 */
@Data
public class StudioVO extends Studio {

    @ApiModelProperty(value = "直播间商品列表")
    private List<Commodity> commodityList;

}
