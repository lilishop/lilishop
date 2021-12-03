package cn.lili.modules.statistics.service;

import cn.hutool.core.date.DateTime;
import cn.lili.modules.store.entity.dos.Bill;
import cn.lili.modules.store.entity.dto.BillSearchParams;
import cn.lili.modules.store.entity.enums.BillStatusEnum;
import cn.lili.modules.store.entity.vos.BillListVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import org.springframework.cache.annotation.CacheConfig;

import javax.servlet.http.HttpServletResponse;
import java.util.Date;

/**
 * 结算单统计
 *
 * @author Chopper
 * @since 2020/11/17 4:28 下午
 */
public interface BillStatisticsService extends IService<Bill> {

    /**
     * 商家待结算数量
     *
     * @param billStatusEnum 结算单类型
     * @return 待结算商家数量
     */
    Integer billNum(BillStatusEnum billStatusEnum);
}