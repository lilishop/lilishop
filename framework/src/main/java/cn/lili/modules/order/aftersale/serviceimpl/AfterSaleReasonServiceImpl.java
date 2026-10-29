package cn.lili.modules.order.aftersale.serviceimpl;

import cn.lili.modules.order.aftersale.entity.dos.AfterSaleReason;
import cn.lili.modules.order.aftersale.mapper.AfterSaleReasonMapper;
import cn.lili.modules.order.aftersale.service.AfterSaleReasonService;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.common.sensitive.SensitiveFilterScene;
import cn.lili.common.sensitive.SensitiveWordsHelper;
import cn.lili.common.vo.PageVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 售后原因业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 7:38 下午
 */
@Service
public class AfterSaleReasonServiceImpl extends ServiceImpl<AfterSaleReasonMapper, AfterSaleReason> implements AfterSaleReasonService {


    @Override
    public boolean save(AfterSaleReason entity) {
        //过滤售后原因敏感词
        entity.setReason(SensitiveWordsHelper.filter(SensitiveFilterScene.REASON, entity.getReason()));
        return super.save(entity);
    }

    @Override
    public List<AfterSaleReason> afterSaleReasonList(String serviceType) {
        LambdaQueryWrapper<AfterSaleReason> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(AfterSaleReason::getServiceType, serviceType);
        return this.list(lambdaQueryWrapper);
    }

    @Override
    public IPage<AfterSaleReason> pageByServiceType(PageVO pageVO, String serviceType) {
        LambdaQueryWrapper<AfterSaleReason> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(AfterSaleReason::getServiceType, serviceType);
        return this.page(PageUtil.initPage(pageVO), lambdaQueryWrapper);
    }

    @Override
    public AfterSaleReason editAfterSaleReason(AfterSaleReason afterSaleReason) {
        //过滤售后原因敏感词
        afterSaleReason.setReason(SensitiveWordsHelper.filter(SensitiveFilterScene.REASON, afterSaleReason.getReason()));
        LambdaUpdateWrapper<AfterSaleReason> lambdaQueryWrapper = Wrappers.lambdaUpdate();
        lambdaQueryWrapper.eq(AfterSaleReason::getId, afterSaleReason.getId());
        lambdaQueryWrapper.set(AfterSaleReason::getReason, afterSaleReason.getReason());
        lambdaQueryWrapper.set(AfterSaleReason::getServiceType, afterSaleReason.getServiceType());
        this.update(lambdaQueryWrapper);
        return afterSaleReason;
    }
}
