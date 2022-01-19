package cn.lili.modules.store.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.store.entity.dos.FreightTemplate;
import cn.lili.modules.store.entity.vos.FreightTemplateVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;

import java.util.List;

/**
 * 店铺地址（自提点）详细业务层
 *
 * @author Bulbasaur
 * @since 2020-03-07 09:24:33
 */
@CacheConfig(cacheNames = "{freightTemplate}")
public interface FreightTemplateService extends IService<FreightTemplate> {

    /**
     * 获取当前商家的运费模板列表
     *
     * @param pageVO 分页
     * @return 运费模板列表
     */
    IPage<FreightTemplate> getFreightTemplate(PageVO pageVO);

    /**
     * 获取商家的运费模板
     *
     * @param storeId
     * @return 运费模板列表
     */
    List<FreightTemplateVO> getFreightTemplateList(String storeId);

    /**
     * 获取运费模板详细信息
     *
     * @param id 运费模板ID
     * @return 运费模板
     */
    @Cacheable(key = "#id")
    FreightTemplateVO getFreightTemplate(String id);

    /**
     * 添加商家运费模板
     * 运费模板分为卖家包邮、运费计算两种类型
     *
     * @param freightTemplateVO 运费模板
     * @return 运费模板
     */
    FreightTemplateVO addFreightTemplate(FreightTemplateVO freightTemplateVO);

    /**
     * 修改商家运费模板
     *
     * @param freightTemplateVO 运费模板
     * @return 运费模板
     */
    @CacheEvict(key = "#freightTemplateVO.id")
    FreightTemplateVO editFreightTemplate(FreightTemplateVO freightTemplateVO);

    /**
     * 删除商家运费模板
     * 删除模板并删除模板的配置内容
     *
     * @param id 运费模板ID
     * @return 操作状态
     */
    @CacheEvict(key = "#freightTemplateVO.id")
    boolean removeFreightTemplate(String id);

}