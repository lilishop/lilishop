package cn.lili.modules.goods.mapper;

import cn.lili.modules.goods.entity.dos.CardKey;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 卡密池 Mapper
 *
 * @author Mike
 * @date 2026-07-31
 */
public interface CardKeyMapper extends BaseMapper<CardKey> {

    /**
     * 并发发卡：按导入时间 FIFO 锁定可售卡密
     */
    @Select("SELECT * FROM li_card_key WHERE sku_id = #{skuId} AND status = 'UNUSED' AND delete_flag = 0 "
            + "ORDER BY create_time ASC LIMIT #{limit} FOR UPDATE")
    List<CardKey> selectForAllocate(@Param("skuId") String skuId, @Param("limit") int limit);

    /**
     * 拼团失败等场景：锁定订单已预占卡密
     */
    @Select("SELECT * FROM li_card_key WHERE order_sn = #{orderSn} AND status = 'RESERVED' AND delete_flag = 0 FOR UPDATE")
    List<CardKey> selectReservedByOrder(@Param("orderSn") String orderSn);
}
