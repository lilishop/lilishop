package cn.lili.modules.store.serviceimpl;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.store.entity.dos.FreightTemplate;
import cn.lili.modules.store.entity.dos.FreightTemplateChild;
import cn.lili.modules.store.entity.vos.FreightTemplateVO;
import cn.lili.modules.store.mapper.FreightTemplateMapper;
import cn.lili.modules.store.service.FreightTemplateChildService;
import cn.lili.modules.store.service.FreightTemplateService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

/**
 * 店铺运费模板业务层实现
 *
 * @author Bulbasaur
 * @since 2020/11/22 16:00
 */
@Service
public class FreightTemplateServiceImpl extends ServiceImpl<FreightTemplateMapper, FreightTemplate> implements FreightTemplateService {
    /**
     * 配送子模板
     */
    @Autowired
    private FreightTemplateChildService freightTemplateChildService;
    /**
     * 缓存
     */
    @Autowired
    private Cache cache;


    @Override
    public List<FreightTemplateVO> getFreightTemplateList(String storeId) {
        //先从缓存中获取运费模板，如果有则直接返回，如果没有则查询数据后再返回
        List<FreightTemplateVO> list = (List<FreightTemplateVO>) cache.get(CachePrefix.SHIP_TEMPLATE.getPrefix() + storeId);
        if (list != null) {
            return list;
        }
        list = new ArrayList<>();
        //查询运费模板
        LambdaQueryWrapper<FreightTemplate> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(FreightTemplate::getStoreId, storeId);
        List<FreightTemplate> freightTemplates = this.baseMapper.selectList(lambdaQueryWrapper);
        if (!freightTemplates.isEmpty()) {
            //如果模板不为空则查询子模板信息
            for (FreightTemplate freightTemplate : freightTemplates) {
                FreightTemplateVO freightTemplateVO = new FreightTemplateVO();
                BeanUtil.copyProperties(freightTemplate, freightTemplateVO);
                List<FreightTemplateChild> freightTemplateChildren = freightTemplateChildService.getFreightTemplateChild(freightTemplate.getId());
                if (!freightTemplateChildren.isEmpty()) {
                    freightTemplateVO.setFreightTemplateChildList(freightTemplateChildren);
                }
                list.add(freightTemplateVO);
            }
        }
        cache.put(CachePrefix.SHIP_TEMPLATE.getPrefix() + storeId, list);
        return list;

    }

    @Override
    public IPage<FreightTemplate> getFreightTemplate(PageVO pageVo) {
        LambdaQueryWrapper<FreightTemplate> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(FreightTemplate::getStoreId, UserContext.getCurrentUser().getStoreId());
        return this.baseMapper.selectPage(PageUtil.initPage(pageVo), lambdaQueryWrapper);
    }

    @Override
    public FreightTemplateVO getFreightTemplate(String id) {
        FreightTemplateVO freightTemplateVO = new FreightTemplateVO();
        //获取运费模板
        FreightTemplate freightTemplate = this.getById(id);
        if (freightTemplate != null) {
            //复制属性
            BeanUtils.copyProperties(freightTemplate, freightTemplateVO);
            //填写运费模板子内容
            List<FreightTemplateChild> freightTemplateChildList = freightTemplateChildService.getFreightTemplateChild(id);
            freightTemplateVO.setFreightTemplateChildList(freightTemplateChildList);
        }
        return freightTemplateVO;
    }

    @Override
    public FreightTemplateVO addFreightTemplate(FreightTemplateVO freightTemplateVO) {
        //获取当前登录商家账号
        AuthUser tokenUser = UserContext.getCurrentUser();
        FreightTemplate freightTemplate = new FreightTemplate();
        //设置店铺ID
        freightTemplateVO.setStoreId(tokenUser.getStoreId());
        //复制属性
        BeanUtils.copyProperties(freightTemplateVO, freightTemplate);
        //添加运费模板
        this.save(freightTemplate);
        //给子模板赋父模板的id
        List<FreightTemplateChild> list = new ArrayList<>();
        //如果子运费模板不为空则进行新增
        if (freightTemplateVO.getFreightTemplateChildList() != null) {
            for (FreightTemplateChild freightTemplateChild : freightTemplateVO.getFreightTemplateChildList()) {
                freightTemplateChild.setFreightTemplateId(freightTemplate.getId());
                list.add(freightTemplateChild);
            }
            //添加运费模板子内容
            freightTemplateChildService.addFreightTemplateChild(list);
        }

        //更新缓存
        cache.remove(CachePrefix.SHIP_TEMPLATE.getPrefix() + tokenUser.getStoreId());
        return freightTemplateVO;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public FreightTemplateVO editFreightTemplate(FreightTemplateVO freightTemplateVO) {
        //获取当前登录商家账号
        AuthUser tokenUser = UserContext.getCurrentUser();
        if (freightTemplateVO.getId().equals(tokenUser.getStoreId())) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        FreightTemplate freightTemplate = new FreightTemplate();
        //复制属性
        BeanUtils.copyProperties(freightTemplateVO, freightTemplate);
        //修改运费模板
        this.updateById(freightTemplate);
        //删除模板子内容
        freightTemplateChildService.removeFreightTemplate(freightTemplateVO.getId());
        //给子模板赋父模板的id
        List<FreightTemplateChild> list = new ArrayList<>();
        for (FreightTemplateChild freightTemplateChild : freightTemplateVO.getFreightTemplateChildList()) {
            freightTemplateChild.setFreightTemplateId(freightTemplate.getId());
            list.add(freightTemplateChild);
        }
        //添加模板子内容
        freightTemplateChildService.addFreightTemplateChild(list);
        //更新缓存
        cache.remove(CachePrefix.SHIP_TEMPLATE.getPrefix() + tokenUser.getStoreId());
        return null;
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean removeFreightTemplate(String id) {
        //获取当前登录商家账号
        AuthUser tokenUser = UserContext.getCurrentUser();
        LambdaQueryWrapper<FreightTemplate> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(FreightTemplate::getStoreId, tokenUser.getStoreId());
        lambdaQueryWrapper.eq(FreightTemplate::getId, id);
        //如果删除成功则删除运费模板子项
        if (this.remove(lambdaQueryWrapper)) {
            cache.remove(CachePrefix.SHIP_TEMPLATE.getPrefix() + tokenUser.getStoreId());
            return freightTemplateChildService.removeFreightTemplate(id);
        }
        return false;
    }
}