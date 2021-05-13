package cn.lili.modules.system.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.system.entity.dos.InstantDelivery;
import cn.lili.modules.system.entity.vo.InstantDeliveryVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 即时配送业务层
 *
 * @author Chopper
 * @date 2020/11/17 8:02 下午
 */
public interface InstantDeliveryService extends IService<InstantDelivery> {

    /**
     * 获取即时配送方案
     *
     * @param page   数据库即时配送方案
     * @param pageVO 分页数据
     * @return
     */
    IPage<InstantDeliveryVO> getInstantDeliveryPage(IPage<InstantDelivery> page, PageVO pageVO);

    /**
     * 根据beanId查询即时配送方案
     *
     * @param bean beanId
     * @return
     */
    InstantDeliveryVO getInstantDeliveryConfig(String bean);

    /**
     * 开启某一个即时配送方案
     *
     * @param bean bean
     * @return
     */
    void openInstantDelivery(String bean);

    /**
     * 修改即时配送方案
     *
     * @param instantDeliveryVO 即时配送方案
     * @return
     */
    InstantDeliveryVO edit(InstantDeliveryVO instantDeliveryVO);

}