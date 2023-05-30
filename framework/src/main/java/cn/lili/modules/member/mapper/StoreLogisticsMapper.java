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
    @Select("SELECT l.id as logistics_id,l.name FROM li_logistics l RIGHT JOIN  li_store_logistics sl ON l.id=sl.logistics_id WHERE sl.store_id=#{storeId} AND l.disabled='OPEN'")
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
    @Select("SELECT id as logistics_id , `name` , ( SELECT sl.id FROM li_store_logistics sl WHERE l.id = sl.logistics_id AND sl.store_id=#{storeId} ) AS selected,(SELECT sl.face_sheet_flag FROM li_store_logistics sl WHERE l.id = sl.logistics_id AND sl.store_id = #{storeId}) as face_sheet_flag FROM li_logistics l WHERE l.disabled='OPEN';")
    //@Select("SELECT *, ( SELECT sl.id FROM li_store_logistics sl WHERE l.id = sl.logistics_id AND sl.store_id=#{storeId} ) AS selected FROM li_logistics l WHERE l.disabled='OPEN';")
    List<StoreLogisticsVO> getStoreLogistics(String storeId);

    /**
     * 店铺已选择的物流公司
     * @param storeId 店铺Id
     * @return 物流公司列表
     */
    @Select("SELECT sl.logistics_id,l.name,sl.face_sheet_flag FROM li_logistics l INNER JOIN li_store_logistics sl on sl.logistics_id=l.id WHERE l.disabled = 'OPEN' AND store_id=#{storeId};")
    List<StoreLogisticsVO> getOpenStoreLogistics(String storeId);

    /**
     * 店铺未选择的物流公司
     * @param storeId 店铺Id
     * @return 物流公司列表
     */
    @Select("SELECT id as logistics_id,name FROM li_logistics WHERE id not in(SELECT logistics_id FROM li_store_logistics WHERE store_id=#{storeId}) AND disabled = 'OPEN'")
    List<StoreLogisticsVO> getCloseStroreLogistics(String storeId);

    /**
     * 获取店铺选择的物流公司并使用了电子面单
     *
     * @param storeId 店铺ID
     * @return 物流公司列表
     */
    @Select("SELECT id as logistics_id , `name` FROM li_logistics WHERE disabled='OPEN'" +
            "AND id in(SELECT logistics_id FROM li_store_logistics WHERE store_id=#{storeId} and face_sheet_flag=1)")
    List<StoreLogisticsVO> getSelectedStoreLogisticsUseFaceSheet(String storeId);


}