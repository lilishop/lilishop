package cn.lili.modules.store.serviceimpl;

import cn.lili.modules.store.entity.dos.FreightTemplateChild;
import cn.lili.modules.store.mapper.FreightTemplateChildMapper;
import cn.lili.modules.store.service.FreightTemplateChildService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 配送子模板业务层实现
 *
 * @author Bulbasaur
 * @since 2020-03-07 09:24:33
 */
@Service
public class FreightTemplateServiceChildImpl extends ServiceImpl<FreightTemplateChildMapper, FreightTemplateChild> implements FreightTemplateChildService {

    @Override
    public List<FreightTemplateChild> getFreightTemplateChild(String freightTemplateId) {
        LambdaQueryWrapper<FreightTemplateChild> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(FreightTemplateChild::getFreightTemplateId, freightTemplateId);
        return this.baseMapper.selectList(lambdaQueryWrapper);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean addFreightTemplateChild(List<FreightTemplateChild> freightTemplateChildren) {
        return this.saveBatch(freightTemplateChildren);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean removeFreightTemplate(String freightTemplateId) {
        LambdaQueryWrapper<FreightTemplateChild> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(FreightTemplateChild::getFreightTemplateId, freightTemplateId);
        return this.remove(lambdaQueryWrapper);
    }


}
