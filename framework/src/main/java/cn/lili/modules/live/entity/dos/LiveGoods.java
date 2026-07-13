package cn.lili.modules.live.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@Schema(title = "直播间商品")
@TableName("li_live_goods")
public class LiveGoods extends BaseEntity {

    @Schema(title = "商品ID")
    private String goodsId;

    @Schema(title = "SKU ID")
    private String skuId;

    @Schema(title = "商品名称")
    private String goodsName;

    @Schema(title = "库存数量")
    private String stock;

    @Schema(title = "商品价格")
    private Double price;

    @Schema(title = "商品图片")
    private String thumbnail;

    @Schema(title = "销售量")
    private Integer salesCount;

    @Schema(title = "店铺ID")
    private String storeId;

    @Schema(title = "店铺名称")
    private String storeName;

    @Schema(title = "原价")
    private Double originPrice;

    @Schema(title = "直播流ID")
    private String liveRoomId;

    @Schema(title = "卖点")
    private String sellPoint;

    @Schema(title = "热度")
    private Integer popularity;

    @Schema(title = "是否隐藏")
    private Boolean hideFlag;

    @Schema(title = "是否可购买")
    private Boolean canBuyFlag;

    @Schema(title = "是否售罄")
    private Boolean soldOutFlag;

    @Schema(title = "是否推荐")
    private Boolean recommend;

    public Boolean getRecommend(){
        if(recommend == null){
            return false;
        }
        return recommend;
    }

    public Boolean getSellOutFlag(){
        if(soldOutFlag == null){
            return false;
        }
        return soldOutFlag;
    }

    public Boolean getCanBuyFlag(){
        if(canBuyFlag == null){
            return false;
        }
        return canBuyFlag;
    }

    public Boolean getHideFlag(){
        if(hideFlag == null){
            return false;
        }
        return hideFlag;
    }

    public Integer getPopularity(){
        if(popularity == null){
            return 0;
        }
        return popularity;
    }

}
