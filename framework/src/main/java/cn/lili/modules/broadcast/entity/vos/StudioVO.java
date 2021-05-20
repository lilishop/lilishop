package cn.lili.modules.broadcast.entity.vos;

import cn.lili.modules.broadcast.entity.dos.Commodity;
import cn.lili.modules.broadcast.entity.dos.Studio;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * @author liushuai(liushuai711 @ gmail.com)
 * @version v4.1
 * @Description:
 * @since 2021/5/17 3:04 下午
 */
@Data
public class StudioVO extends Studio {

    @ApiModelProperty(value = "直播间商品列表")
    private List<Commodity> CommodityList;

}
