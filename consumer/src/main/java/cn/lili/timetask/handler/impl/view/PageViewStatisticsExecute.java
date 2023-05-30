package cn.lili.timetask.handler.impl.view;

import cn.hutool.core.convert.Convert;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.statistics.entity.dos.PlatformViewData;
import cn.lili.modules.statistics.service.PlatformViewService;
import cn.lili.timetask.handler.EveryDayExecute;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

/**
 * 统计 入库
 *
 * @author Chopper
 * @since 2021-01-15 18:20
 */
@Slf4j
@Component
public class PageViewStatisticsExecute implements EveryDayExecute {
    /**
     * 缓存
     */
    @Autowired
    private Cache cache;
    /**
     * 平台PV统计
     */
    @Autowired
    private PlatformViewService platformViewService;

    @Override
    public void execute() {

        //1、缓存keys 模糊匹配
        //2、过滤今日的数据，即今天只能统计今日以前的数据
        //4对key value 分别代表平台PV、平台UV、店铺PV、店铺UV
        List<String> pvKeys = filterKeys(cache.keys(CachePrefix.PV.getPrefix() + "*"));
        List<Integer> pvValues = cache.multiGet(pvKeys);

        List<String> storePVKeys = filterKeys(cache.keys(CachePrefix.STORE_PV.getPrefix() + "*"));
        List<Integer> storePvValues = cache.multiGet(storePVKeys);

        //备份UV数据，这里赋值之后，会被删除
        List<String> uvKeys = new ArrayList<>();
        List<String> storeUvKeys = new ArrayList<>();

        log.debug("开始统计平台数据，PV共计【{}】条", pvKeys.size());
        log.debug("开始统计店铺数据，PV共计【{}】条", storePvValues.size());

        //定义要统计的数据
        List<PlatformViewData> platformViewDataList = new ArrayList<>();

        //PV 统计
        if (pvKeys.size() > 0) {
            for (int i = 0; i < pvKeys.size(); i++) {
                String key = pvKeys.get(i);
                PageViewStatistics pageViewStatistics = new PageViewStatistics(key);
                PlatformViewData platformPVData = new PlatformViewData();
                BeanUtil.copyProperties(pageViewStatistics, platformPVData);
                platformPVData.setPvNum(pvValues.get(i).longValue());
                //根据pvkey 获取 uvkey
                String uvKey = getUvKey(key);
                uvKeys.add(uvKey);
                platformPVData.setUvNum(cache.counter(uvKey));
                platformPVData.setStoreId("-1");
                platformViewDataList.add(platformPVData);
            }
            batchSave(pvKeys, uvKeys, platformViewDataList);
        }
        //店铺 PV 统计
        if (storePVKeys.size() > 0) {
            platformViewDataList = new ArrayList<>();
            for (int i = 0; i < storePVKeys.size(); i++) {
                String key = storePVKeys.get(i);
                PageViewStatistics pageViewStatistics = new PageViewStatistics(key);
                PlatformViewData storePVData = new PlatformViewData();
                BeanUtil.copyProperties(pageViewStatistics, storePVData);
                storePVData.setPvNum(storePvValues.get(i).longValue());
                //根据pvkey 获取 uvkey
                String uvKey = getUvKey(key);
                uvKeys.add(uvKey);
                storePVData.setUvNum(cache.counter(uvKey));
                platformViewDataList.add(storePVData);
            }
            batchSave(storePVKeys, storeUvKeys, platformViewDataList);
        }
    }

    /**
     * 根据缓存的PVkey 获取对应的UVkey
     *
     * @param key
     * @return
     */
    private String getUvKey(String key) {
        if (StringUtils.isNotEmpty(key)) {

            key = key.replace(CachePrefix.PV.getPrefix(), CachePrefix.UV.getPrefix());
            key = key.replace(CachePrefix.STORE_PV.getPrefix(), CachePrefix.STORE_UV.getPrefix());
            return key;
        }
        return key;
    }

    /**
     * 批量保存数据&&清除保存数据的缓存
     *
     * @param pvKeys           PV key
     * @param uvKeys           UV key
     * @param platformViewData DOS
     */
    @Transactional(rollbackFor = Exception.class)
    void batchSave(List<String> pvKeys, List<String> uvKeys, List<PlatformViewData> platformViewData) {
        log.debug("批量保存流量数据，共计【{}】条", platformViewData.size());
        platformViewService.saveBatch(platformViewData);
        //批量删除缓存key
        cache.multiDel(pvKeys);
        cache.multiDel(uvKeys);
        log.debug("流量数据保存完成");
    }


    /**
     * 过滤缓存key
     *
     * @param keys 缓存key集合
     */
    private static List<String> filterKeys(List<String> keys) {

        //只统计一天前的数据
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.HOUR_OF_DAY, -24);

        List<String> result = new ArrayList<>();
        for (String key : keys) {
            PageViewStatistics temp = new PageViewStatistics(key);
            //如果需要统计，则将key写入集合
            if (temp.getDate().before(calendar.getTime())) {
                result.add(key);
            }
        }

        return result;
    }

}

/**
 * 根据缓存key 获取其中需要的参数，年月日，以及店铺信息
 */
@Data
class PageViewStatistics {
    /**
     * 年 、 月 、 日 、 店铺id
     */
    private Date date;
    private String storeId;

    public PageViewStatistics(String str) {
        //将字符串解析成需要的对象
        str = str.substring(str.indexOf("}") + 2);
        String[] dateStr = str.split("-");
        Integer year = Convert.toInt(dateStr[0]);
        Integer month = Convert.toInt(dateStr[1]);
        Integer day;
        //是否有店铺id
        if (dateStr.length > 3) {
            day = Convert.toInt(dateStr[2]);
            this.storeId = dateStr[3];
        } else {
            day = Convert.toInt(dateStr[2]);
        }
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.YEAR, year);
        calendar.set(Calendar.MONTH, month - 1);
        calendar.set(Calendar.DAY_OF_MONTH, day);
        calendar.set(Calendar.HOUR_OF_DAY, 0);
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        this.date = calendar.getTime();
    }

}
