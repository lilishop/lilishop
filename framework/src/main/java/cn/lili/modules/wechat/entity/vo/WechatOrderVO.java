package cn.lili.modules.wechat.entity.vo;

import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.beans.BeanUtils;

import java.util.Date;

/**
 * 平台小程序订单专用列表视图；继承通用订单展示字段，独立承载微信发货同步信息。
 * 复用现有订单查询结果进行转换，不向通用订单接口增加微信专有字段。
 *
 * @author Bulbasaur
 * @since 2026-11-09
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(description = "小程序订单及微信发货同步信息")
public class WechatOrderVO extends OrderSimpleVO {
    /** 微信发货同步状态：UNCHECKED 待核查、PENDING 待上报、PROCESSING 处理中、SUCCESS 成功、FAILED 失败。 */
    @Schema(description = "微信发货同步状态", allowableValues = {"UNCHECKED", "PENDING", "PROCESSING", "SUCCESS", "FAILED"})
    private String wechatSyncStatus;

    /** 最近一次微信发货同步尝试时间，Asia/Shanghai；无本地记录时为空，不等同于上报成功时间。 */
    @Schema(description = "最近一次微信上报尝试时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date wechatSyncTime;

    /** 最近一次微信发货同步的脱敏处理原因；仅管理端小程序订单接口返回。 */
    @Schema(description = "微信上报失败或待处理原因")
    private String wechatSyncError;

    /**
     * 从通用查询结果复制订单字段，保留原商品规格及展示计算规则，不修改来源对象。
     * @param order 已经完成通用字段回填的订单列表记录
     */
    public WechatOrderVO(OrderSimpleVO order) {
        BeanUtils.copyProperties(order, this);
    }
}
