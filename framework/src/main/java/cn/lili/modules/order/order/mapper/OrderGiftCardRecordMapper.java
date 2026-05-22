package cn.lili.modules.order.order.mapper;

import cn.lili.modules.order.order.entity.dos.OrderGiftCardRecord;
import cn.lili.modules.promotion.entity.vos.GiftCardUsageRecordVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

/**
 * 订单礼品卡使用流水数据访问接口（含使用记录分页联表查询）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
public interface OrderGiftCardRecordMapper extends BaseMapper<OrderGiftCardRecord> {

    @Select("SELECT ogr.*, m.mobile AS memberMobile, IFNULL(m.nick_name, m.username) AS memberName, " +
            "a.gift_card_name AS giftCardName, c.face_value AS faceValue, c.balance AS balance " +
            "FROM li_order_gift_card_record ogr " +
            "LEFT JOIN li_member m ON m.id = ogr.member_id AND m.delete_flag = 0 " +
            "LEFT JOIN li_gift_card_cash_credential c ON c.id = ogr.credential_id AND c.delete_flag = 0 " +
            "LEFT JOIN li_gift_card_cash_activity a ON a.id = c.activity_id AND a.delete_flag = 0 ${ew.customSqlSegment}")
    IPage<GiftCardUsageRecordVO> pageUsageRecord(IPage<GiftCardUsageRecordVO> page,
                                                 @Param(Constants.WRAPPER) Wrapper<GiftCardUsageRecordVO> queryWrapper);
}
