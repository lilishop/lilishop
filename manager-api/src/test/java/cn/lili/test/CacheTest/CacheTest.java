package cn.lili.test.CacheTest;

import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.modules.statistics.util.StatisticsSuffix;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.redis.core.DefaultTypedTuple;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Date;
import java.util.Random;
import java.util.Set;

/**
 * @author Chopper
 * @version v1.0
 * @since v7.0
 * 2021/1/15 16:25
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest
class CacheTest {

    @Autowired
    private Cache cache;

    String KEY = "test1";

    /**
     * 计数器测试
     *
     * @throws InterruptedException
     */
    @Test
    void testCache() throws InterruptedException {

        System.out.println(cache.incr(KEY, 3));
        System.out.println(cache.incr(KEY, 1));
        System.out.println(cache.incr(KEY, 1));
        Thread.sleep(2000);
        System.out.println(cache.incr(KEY, 1));
        Thread.sleep(1000);
        System.out.println(cache.incr(KEY, 1));
        Thread.sleep(10000);

        System.out.println(cache.incr(KEY, 1));

    }

    /**
     * 缓存中的流量统计数据模拟
     */
    @Test
    void pageViewInit() {
        String storeUV = "{STORE_UV}_2021-4-12-1376369067769724928";
        String storePV = "{STORE_PV}_2021-4-12-1376369067769724928";
        String UV = "{UV}_2021-4-12";
        String PV = "{PV}_2021-4-12";
        Random random = new Random();
        for (int i = 0; i < 1000; i++) {
            //PV
            cache.incr(PV, 60 * 60 * 48);
            cache.incr(storePV, 60 * 60 * 48);
            //店铺UV 统计，则需要对id去重复，所以如下处理
            cache.cumulative(storeUV, "192.168.0.1" + random.nextInt(100));
            //平台UV统计
            cache.cumulative(UV, "192.168.0.1" + random.nextInt(100));
        }
    }


    /**
     * 流量单元测试
     * <p>
     * 模拟1000次请求发，查看这块执行时间,单节点性能简单尝试同时还有redis连接池问题，这个只是简单压力模拟
     * <p>
     * 执行结果
     * 1.251
     * 1.167
     * 1.363
     */
    @Test
    void testPageViewStatistics() {
        Date start = new Date();
        System.out.println(start.getTime());
        for (int i = 0; i < 1000; i++) {

            //PV 统计48小时过期 留下一定时间予以统计累计数据库
            cache.incr(CachePrefix.PV.getPrefix() + StatisticsSuffix.suffix(i / 100 + ""), 60 * 60 * 48);

            //店铺UV 统计，则需要对id去重复，所以如下处理
            cache.cumulative(CachePrefix.UV.getPrefix() + StatisticsSuffix.suffix(i / 100 + "1321312312312312321312"), "192.168.0.1" + i);

            //平台UV统计
            cache.cumulative(CachePrefix.UV.getPrefix() + StatisticsSuffix.suffix(), "192.168.0.1" + i);
        }

        Date end = new Date();
        System.out.println(end.getTime());
        System.out.println(end.getTime() - start.getTime());
    }

    @Test
    void testZincrby() {
        cache.incrementScore("searchHotWord", "Chrome");
        Assertions.assertTrue(true);
    }

    @Test
    void testReverseRangeWithScores() {
        Set searchHotWord = cache.reverseRangeWithScores("searchHotWord", 0, 100);
        for (Object o : searchHotWord) {
            DefaultTypedTuple str = (DefaultTypedTuple) o;
            System.out.println(str.getScore());
            System.out.println(str.getValue());
            System.out.println("----------");
        }
        Assertions.assertTrue(true);
    }


    @Test
    void scanTests() {

        for (int i = 0; i < 1000000; i++) {
            cache.put("scan" + i, i);
        }
        Date date = new Date();
        System.out.println(JSONUtil.toJsonStr(cache.keys("scan999999*")));

        System.out.println("100w数据耗时");
        System.out.println(new Date().getTime() - date.getTime());


        for (int i = 1000000; i < 5000000; i++) {
            cache.put("scan" + i, i);
        }

        date = new Date();
        System.out.println(JSONUtil.toJsonStr(cache.keys("scan999999*")));
        System.out.println("600w数据耗时");
        System.out.println(new Date().getTime() - date.getTime());

    }

    // scan 慢些，但是在缓存更多的情况下，表现更好，虽然用时更久，但是不会阻塞其他读写
    //
    //["scan999999"]
    //redisTemplate scan 500w数据耗时
    //2985
    //["scan999999"]
    //redisTemplate keys 500w数据耗时
    //1073
    @Test
    void testKsysVsScan() {
        Date date = new Date();
        System.out.println(JSONUtil.toJsonStr(cache.keys("scan999999*")));
        System.out.println("redisTemplate scan 500w数据耗时");
        System.out.println(new Date().getTime() - date.getTime());

        date = new Date();
        System.out.println(JSONUtil.toJsonStr(cache.keysBlock("scan999999*")));
        System.out.println("redisTemplate keys 500w数据耗时");
        System.out.println(new Date().getTime() - date.getTime());

    }

}
