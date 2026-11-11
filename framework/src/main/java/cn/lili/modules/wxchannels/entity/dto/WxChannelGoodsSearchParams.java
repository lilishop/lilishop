package cn.lili.modules.wxchannels.entity.dto;

import cn.hutool.core.text.CharSequenceUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import cn.lili.modules.wxchannels.entity.dos.WxChannelGoods;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = false)
public class WxChannelGoodsSearchParams extends cn.lili.common.vo.PageVO {

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "状态 APPROVED,PENDING,REJECTED")
    private String status;

    public QueryWrapper<WxChannelGoods> queryWrapper() {
        QueryWrapper<WxChannelGoods> wrapper = new QueryWrapper<>();
        wrapper.like(CharSequenceUtil.isNotEmpty(goodsName), "goods_name", goodsName);
        wrapper.eq(CharSequenceUtil.isNotEmpty(status), "status", status);
        wrapper.eq("delete_flag", false);
        return wrapper;
    }
}
