package cn.lili.modules.system.service;

import cn.lili.modules.system.entity.dos.Region;
import cn.lili.modules.system.entity.vo.RegionVO;
import com.baomidou.mybatisplus.extension.service.IService;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;

import java.util.List;
import java.util.Map;

/**
 * 行政地区业务层
 *
 * @author Chopper
 * @since 2020/12/2 14:14
 */
@CacheConfig(cacheNames = "{regions}")
public interface RegionService extends IService<Region> {

    /**
     * 同步行政数据
     *
     * @param url
     */
    @CacheEvict
    void synchronizationData(String url);

    /**
     * 获取地区列表
     *
     * @param id 地区ID
     * @return 地区列表
     */
    @Cacheable(key = "#id")
    List<Region> getItem(String id);

    /**
     * 根据最后一级名称获取改所有上级地区id
     *
     * @param lastName 最后一级名称
     * @return 全部地区id
     */
    String getItemByLastName(String lastName);

    /**
     * 获取地址
     *
     * @param cityCode 城市编码
     * @param townName 镇名称
     * @return
     */
    Map<String, Object> getRegion(String cityCode, String townName);

    /**
     * 获取所有的城市
     *
     * @return
     */
    @Cacheable(key = "'ALL_CITY'")
    List<RegionVO> getAllCity();
}