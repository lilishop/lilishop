package cn.lili.modules.store.mapper;

import cn.hutool.core.date.DateTime;
import cn.lili.modules.store.entity.dos.StoreDetail;
import cn.lili.modules.store.entity.dto.StoreAfterSaleAddressDTO;
import cn.lili.modules.store.entity.dto.StoreSettlementDay;
import cn.lili.modules.store.entity.vos.StoreBasicInfoVO;
import cn.lili.modules.store.entity.vos.StoreDetailVO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import org.apache.ibatis.annotations.Update;

import java.util.List;

/**
 * 店铺详细数据处理层
 *
 * @author pikachu
 * @date 2020-03-07 09:18:56
 */
public interface StoreDetailMapper extends BaseMapper<StoreDetail> {

    @Select("select s.store_logo,s.member_name,s.store_name,s.store_disable,s.self_operated,s.store_address_detail,s.store_address_path,s.store_address_id_path,s.store_center,s.store_desc,s.yzf_sign,s.yzf_mp_sign," +
            "d.* from li_store s inner join li_store_detail d on s.id=d.store_id where s.id=#{storeId}")
    StoreDetailVO getStoreDetail(String storeId);

    @Select("select s.member_name,s.store_name,s.store_disable,s.self_operated,s.store_center,s.store_logo,s.store_desc,d.* " +
            "from li_store s inner join li_store_detail d on s.id=d.store_id where s.member_id=#{memberId}")
    StoreDetailVO getStoreDetailByMemberId(String memberId);

    @Select("SELECT s.id as storeId,s.* FROM li_store s WHERE s.id=#{storeId}")
    StoreBasicInfoVO getStoreBasicInfoDTO(String storeId);

    @Select("select s.sales_consignee_name,s.sales_consignee_mobile,s.sales_consignee_address_id,s.sales_consignee_address_path,s.sales_consignee_detail " +
            "from li_store_detail s  where s.store_id=#{storeId}")
    StoreAfterSaleAddressDTO getStoreAfterSaleAddressDTO(String storeId);

    @Select("SELECT store_id,settlement_day FROM li_store_detail " +
            "WHERE settlement_cycle LIKE concat(#{day},',%')  " +
            "OR settlement_cycle LIKE concat('%,',#{day},',%') " +
            "OR settlement_cycle LIKE concat('%,',#{day})")
    List<StoreSettlementDay> getSettlementStore(int day);

    @Update("UPDATE li_store_detail SET settlement_day=#{dateTime} WHERE store_id=#{storeId}")
    void updateSettlementDay(String storeId, DateTime dateTime);
}