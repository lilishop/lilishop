package cn.lili.modules.broadcast.mapper;

import cn.lili.modules.broadcast.entity.dos.Commodity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 直播商品数据层
 *
 * @author Bulbasaur
 * @date: 2021/5/17 9:56 上午
 */
public interface CommodityMapper extends BaseMapper<Commodity> {

    @Select("SELECT live_goods_id FROM li_commodity WHERE audit_status='0' or audit_status='1'")
    List<String> getAuditCommodity();

}