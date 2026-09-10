package cn.lili.modules.distribution.mapper;

import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelation;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerCustomerTradeStatsVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerCustomerVO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.Date;
import java.util.List;

/**
 * 分销客户当前关系数据处理层
 *
 * @author pikachu
 */
public interface DistributionCustomerRelationMapper extends BaseMapper<DistributionCustomerRelation> {

    /** 分销员端客户列表筛选条件 */
    String BUYER_CUSTOMER_FILTER_SQL =
            "FROM li_distribution_customer_relation r "
                    + "LEFT JOIN li_member m ON m.id = r.member_id AND IFNULL(m.delete_flag, 0) = 0 "
                    + "WHERE r.distribution_id = #{distributionId} AND IFNULL(r.delete_flag, 0) = 0 "
                    + "AND r.relation_status &lt;&gt; 'SELF_BIND' "
                    + "<if test='startTime != null'> AND r.bind_time &gt;= #{startTime} </if>"
                    + "<if test='endTime != null'> AND r.bind_time &lt;= #{endTime} </if>"
                    + "<if test='keyword != null and keyword != \"\"'> "
                    + "AND (m.mobile LIKE CONCAT('%', #{keyword}, '%') OR m.nick_name LIKE CONCAT('%', #{keyword}, '%')) "
                    + "</if>"
                    + "<if test='filterType == \"STARRED\"'> AND IFNULL(r.starred, 0) = 1 </if>"
                    + "<if test='filterType == \"ORDERED\"'> "
                    + "AND EXISTS (SELECT 1 FROM li_distribution_order o "
                    + "WHERE o.distribution_id = r.distribution_id AND o.member_id = r.member_id) "
                    + "</if>"
                    + "<if test='filterType == \"EXPIRING_SOON\"'> "
                    + "AND r.relation_status IN ('TEMPORARY', 'PENDING') AND ("
                    + "(r.expire_at IS NOT NULL AND r.expire_at &gt;= NOW() AND r.expire_at &lt;= DATE_ADD(NOW(), INTERVAL 7 DAY)) OR "
                    + "(r.pending_permanent_deadline IS NOT NULL AND r.pending_permanent_deadline &gt;= NOW() "
                    + "AND r.pending_permanent_deadline &lt;= DATE_ADD(NOW(), INTERVAL 7 DAY))"
                    + ") "
                    + "</if>";

    /**
     * 统计分销员客户数量。
     */
    @Select("<script>SELECT COUNT(1) " + BUYER_CUSTOMER_FILTER_SQL + "</script>")
    long countBuyerCustomers(@Param("distributionId") String distributionId,
                             @Param("filterType") String filterType,
                             @Param("keyword") String keyword,
                             @Param("startTime") Date startTime,
                             @Param("endTime") Date endTime);

    /**
     * 分页查询分销员客户列表。
     */
    @Select("<script>SELECT r.id AS id, "
            + "r.member_id AS memberId, "
            + "COALESCE(NULLIF(m.nick_name, ''), m.username, r.member_id) AS memberNickname, "
            + "m.face AS memberAvatar, "
            + "m.mobile AS memberMobile, "
            + "r.relation_status AS relationStatus, "
            + "IFNULL(r.starred, 0) AS starred, "
            + "r.bind_time AS bindTime, "
            + "r.expire_at AS expireAt "
            + BUYER_CUSTOMER_FILTER_SQL
            + "ORDER BY r.bind_time DESC "
            + "LIMIT #{offset}, #{pageSize}</script>")
    List<DistributionBuyerCustomerVO> listBuyerCustomers(@Param("distributionId") String distributionId,
                                                         @Param("filterType") String filterType,
                                                         @Param("keyword") String keyword,
                                                         @Param("startTime") Date startTime,
                                                         @Param("endTime") Date endTime,
                                                         @Param("offset") long offset,
                                                         @Param("pageSize") long pageSize);

    /**
     * 查询指定客户的成交统计。
     */
    @Select("SELECT COUNT(DISTINCT do.order_sn) AS orderCount, "
            + "COALESCE(SUM(sf.final_price), 0) AS tradeAmount, "
            + "MAX(do.create_time) AS lastTradeTime "
            + "FROM li_distribution_order do "
            + "LEFT JOIN li_store_flow sf ON sf.order_item_sn = do.order_item_sn AND sf.flow_type = 'PAY' "
            + "WHERE do.distribution_id = #{distributionId} AND do.member_id = #{memberId} "
            + "AND do.distribution_order_status != 'REFUND'")
    DistributionBuyerCustomerTradeStatsVO selectBuyerCustomerTradeStats(@Param("distributionId") String distributionId,
                                                                      @Param("memberId") String memberId);
}
