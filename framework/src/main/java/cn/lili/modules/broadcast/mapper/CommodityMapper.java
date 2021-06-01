package cn.lili.modules.broadcast.mapper;

import cn.lili.modules.broadcast.entity.dos.Commodity;
import cn.lili.modules.broadcast.entity.dto.SimpleCommodity;
import cn.lili.modules.broadcast.entity.vos.CommodityVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
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

    @Select("SELECT * FROM li_commodity c INNER JOIN li_studio_commodity sc ON sc.goods_id = c.live_goods_id WHERE sc.room_id =#{roomId}")
    List<Commodity> getCommodityByRoomId(Integer roomId);

    @Select("SELECT goods_image FROM li_commodity c INNER JOIN li_studio_commodity sc ON sc.goods_id = c.live_goods_id WHERE sc.room_id =#{roomId}")
    List<String> getSimpleCommodityByRoomId(Integer roomId);

    @Select("SELECT c.*,gs.quantity,s.store_name FROM li_commodity c INNER JOIN li_goods_sku gs ON c.sku_id = gs.id INNER JOIN li_store s ON s.id=c.store_id ${ew.customSqlSegment}")
    IPage<CommodityVO> commodityVOList(IPage<CommodityVO> page, @Param(Constants.WRAPPER) Wrapper<CommodityVO> queryWrapper);

}