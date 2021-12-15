package cn.lili.modules.statistics.serviceimpl;

import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.order.aftersale.entity.enums.ComplaintStatusEnum;
import cn.lili.modules.order.order.entity.dos.OrderComplaint;
import cn.lili.modules.statistics.mapper.OrderComplaintStatisticsMapper;
import cn.lili.modules.statistics.service.OrderComplaintStatisticsService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * 交易投诉业务层实现
 *
 * @author paulG
 * @since 2020/12/5
 **/
@Service
public class OrderComplaintStatisticsServiceImpl extends ServiceImpl<OrderComplaintStatisticsMapper, OrderComplaint> implements OrderComplaintStatisticsService {

    @Override
    public long waitComplainNum() {
        QueryWrapper queryWrapper = Wrappers.query();
        queryWrapper.ne("complain_status", ComplaintStatusEnum.COMPLETE.name());
        queryWrapper.eq(StringUtils.equals(UserContext.getCurrentUser().getRole().name(), UserEnums.STORE.name()),
                "store_id", UserContext.getCurrentUser().getStoreId());
        return this.count(queryWrapper);
    }


}
