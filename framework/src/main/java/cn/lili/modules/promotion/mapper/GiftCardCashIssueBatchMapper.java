package cn.lili.modules.promotion.mapper;

import cn.lili.modules.promotion.entity.dos.GiftCardCashIssueBatch;
import cn.lili.modules.promotion.entity.vos.GiftCardCashIssueBatchVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

/**
 * 现金礼品卡发卡批次数据访问接口（含分页联表查询）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
public interface GiftCardCashIssueBatchMapper extends BaseMapper<GiftCardCashIssueBatch> {

    @Select("SELECT ib.*, ib.id AS issueBatchId, " +
            "CONCAT(IFNULL(cg.min_card_no, ''), '-', IFNULL(cg.max_card_no, '')) AS cardNoRange, " +
            "a.face_value AS faceValue, " +
            "a.gift_card_name AS giftCardName, " +
            "COALESCE(cg.max_expire_time, a.valid_end_time) AS expireTime " +
            "FROM li_gift_card_cash_issue_batch ib " +
            "LEFT JOIN li_gift_card_cash_activity a ON a.id = ib.activity_id AND a.delete_flag = 0 " +
            "LEFT JOIN (" +
            "SELECT issue_batch_id, MIN(card_no) AS min_card_no, MAX(card_no) AS max_card_no, MAX(expire_time) AS max_expire_time " +
            "FROM li_gift_card_cash_credential WHERE delete_flag = 0 GROUP BY issue_batch_id" +
            ") cg ON cg.issue_batch_id = ib.id ${ew.customSqlSegment}")
    IPage<GiftCardCashIssueBatchVO> pageByParams(IPage<GiftCardCashIssueBatchVO> page,
                                                  @Param(Constants.WRAPPER) Wrapper<GiftCardCashIssueBatchVO> queryWrapper);
}
