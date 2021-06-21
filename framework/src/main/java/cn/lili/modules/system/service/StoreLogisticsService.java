package cn.lili.modules.system.service;

import cn.lili.modules.store.entity.dos.StoreLogistics;
import cn.lili.modules.system.entity.vo.StoreLogisticsVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 店铺-物流公司业务层
 *
 * @author Chopper
 * @date 2020/11/17 8:02 下午
 */
public interface StoreLogisticsService extends IService<StoreLogistics> {

    /**
     * 获取当前店铺的物流公司列表
     *
     * @return 物流公司列表
     */
    List<StoreLogisticsVO> getStoreLogistics();

    /**
     * 获取当前店铺已选择的物流公司列表
     *
     * @return 物流公司列表
     */
    List<StoreLogisticsVO> getStoreSelectedLogistics();

    /**
     * 获取当前店铺已选择的物流公司名称列表
     *
     * @return 物流公司列表
     */
    List<String> getStoreSelectedLogisticsName();

    /**
     * 添加店铺-物流公司
     *
     * @param logisticsId
     * @return 店铺物流公司
     */
    StoreLogistics add(String logisticsId);


}