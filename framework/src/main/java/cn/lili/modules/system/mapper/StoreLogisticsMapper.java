package cn.lili.modules.system.mapper;

import cn.lili.modules.store.entity.dos.StoreLogistics;
import cn.lili.modules.system.entity.vo.StoreLogisticsVO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 物流公司数据处理层
 *
 * @author Chopper
 * @date 2020/11/17 8:01 下午
 */
public interface StoreLogisticsMapper extends BaseMapper<StoreLogistics> {

    @Select("SELECT l.* FROM li_logistics l RIGHT JOIN  li_store_logistics sl ON l.id=sl.logistics_id WHERE sl.store_id=#{storeId} AND l.disabled='OPEN'")
    List<StoreLogisticsVO> getSelectedStoreLogistics(String storeId);

    @Select("SELECT l.name FROM li_logistics l RIGHT JOIN  li_store_logistics sl ON l.id=sl.logistics_id WHERE sl.store_id=#{storeId} AND l.disabled='OPEN'")
    List<String> getSelectedStoreLogisticsName(String storeId);

    @Select("SELECT *, ( SELECT sl.id FROM li_store_logistics sl WHERE l.id = sl.logistics_id AND sl.store_id=#{storeId} ) AS selected FROM li_logistics l AND l.disabled='OPEN';")
    List<StoreLogisticsVO> getStoreLogistics(String storeId);

}