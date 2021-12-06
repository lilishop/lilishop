package cn.lili.modules.member.mapper;

import cn.lili.modules.store.entity.dos.StoreLogistics;
import cn.lili.modules.system.entity.vo.StoreLogisticsVO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 物流公司数据处理层
 *
 * @author Chopper
 * @since 2020/11/17 8:01 下午
 */
public interface StoreLogisticsMapper extends BaseMapper<StoreLogistics> {

    /**
     * 获取店铺选择的物流公司
     *
     * @param storeId 店铺ID
     * @return 物流公司列表
     */
    @Select("SELECT l.* FROM li_logistics l RIGHT JOIN  li_store_logistics sl ON l.id=sl.logistics_id WHERE sl.store_id=#{storeId} AND l.disabled='OPEN'")
    List<StoreLogisticsVO> getSelectedStoreLogistics(String storeId);

    /**
     * 店铺已选择的物流公司名称列表
     *
     * @param storeId 店铺ID
     * @return 店铺已选择的物流公司名称列表
     */
    @Select("SELECT l.name FROM li_logistics l RIGHT JOIN  li_store_logistics sl ON l.id=sl.logistics_id WHERE sl.store_id=#{storeId} AND l.disabled='OPEN'")
    List<String> getSelectedStoreLogisticsName(String storeId);

    /**
     * 获取店铺地址VO列表
     *
     * @param storeId 店铺列表
     * @return 店铺地址VO列表
     */
    @Select("SELECT *, ( SELECT sl.id FROM li_store_logistics sl WHERE l.id = sl.logistics_id AND sl.store_id=#{storeId} ) AS selected FROM li_logistics l WHERE l.disabled='OPEN';")
    List<StoreLogisticsVO> getStoreLogistics(String storeId);

}