package cn.lili.modules.broadcast.mapper;

import cn.lili.modules.broadcast.entity.dos.Commodity;
import cn.lili.modules.broadcast.entity.dto.SimpleCommodity;
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

    @Select("SELECT live_goods_id FROM li_commodity WHERE audit_status='0' or audit_status='1' AND store_id =#{storeId}")
    List<String> getAuditCommodity(String storeId);

    @Select("SELECT * FROM li_commodity c WINNER JOIN li_studio_commodity sc ON sc.goods_id = c.live_goods_id WHERE sc.room_id =#{roomId}")
    List<Commodity> getCommodityByRoomId(Integer roomId);

    @Select("SELECT name,goods_image FROM li_commodity c WINNER JOIN li_studio_commodity sc ON sc.goods_id = c.live_goods_id WHERE sc.room_id =#{roomId}")
    List<SimpleCommodity> getSimpleCommodityByRoomId(Integer roomId);

}