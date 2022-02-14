package cn.lili.modules.statistics.aop.aspect;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.utils.IpUtils;
import cn.lili.common.utils.SpelUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.vos.GoodsSkuVO;
import cn.lili.modules.statistics.aop.PageViewPoint;
import cn.lili.modules.statistics.aop.enums.PageViewEnum;
import cn.lili.modules.statistics.util.StatisticsSuffix;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

/**
 * 页面浏览统计拦截
 *
 * @author Chopper
 * @since 2021-01-14 18:01
 */
@Aspect
@Configuration
@Slf4j
public class PageViewInterceptor {

    @Autowired
    private Cache cache;



    @AfterReturning(returning = "rvt", pointcut = "@annotation(cn.lili.modules.statistics.aop.PageViewPoint)")
    public void interceptor(JoinPoint point, Object rvt) {
        MethodSignature signature = (MethodSignature) point.getSignature();
        Method method = signature.getMethod();
        PageViewPoint pageViewPoint = method.getAnnotation(PageViewPoint.class);
        PageViewEnum pageViewEnum = pageViewPoint.type();
        //store id 为-1 代表平台访问
        String storeId;
        //商品访问
        String goodsId = null;

        switch (pageViewEnum) {
            case SKU:
                ResultMessage<Map<String, Object>> skuRvt = (ResultMessage<Map<String, Object>>) rvt;
                if (skuRvt != null && skuRvt.getResult() != null && skuRvt.getResult().containsKey("data")) {
                    GoodsSkuVO goodsSkuDetail = (GoodsSkuVO) skuRvt.getResult().get("data");
                    storeId = goodsSkuDetail.getStoreId();
                    goodsId = goodsSkuDetail.getGoodsId();
                    break;
                }
            case STORE:
                Map<String, String> map = null;
                try {
                    map = spelFormat(point);
                } catch (Exception e) {
                    return;
                }
                storeId = map.get("id");
                break;
            default:
                storeId = "-1";
        }
        String ip = IpUtils.getIpAddress(ThreadContextHolder.getHttpRequest());
        try {
            //PV 统计48小时过期 留下一定时间予以统计累计数据库
            cache.incr(CachePrefix.PV.getPrefix() + StatisticsSuffix.suffix(), 60 * 60 * 48);

            //平台UV统计
            cache.cumulative(CachePrefix.UV.getPrefix() + StatisticsSuffix.suffix(), ip);

            //PV 统计48小时过期 留下一定时间予以统计累计数据库
            cache.incr(CachePrefix.STORE_PV.getPrefix() + StatisticsSuffix.suffix(storeId), 60 * 60 * 48);

            //店铺UV 统计，则需要对id去重复，所以如下处理
            cache.cumulative(CachePrefix.STORE_UV.getPrefix() + StatisticsSuffix.suffix(storeId), ip);
        } catch (Exception e) {
            log.error("页面出错", e);
        }

    }

    /**
     * 获取注解中对方法的描述信息 用于Controller层注解
     *
     * @param joinPoint 切点
     * @return 方法描述
     * @throws Exception
     */
    private static Map<String, String> spelFormat(JoinPoint joinPoint) {

        Map<String, String> result = new HashMap<>(2);
        MethodSignature signature = (MethodSignature) joinPoint.getSignature();
        PageViewPoint pageViewPoint = signature.getMethod().getAnnotation(PageViewPoint.class);
        String id = SpelUtil.compileParams(joinPoint, pageViewPoint.id());
        result.put("id", id);
        return result;
    }
}
