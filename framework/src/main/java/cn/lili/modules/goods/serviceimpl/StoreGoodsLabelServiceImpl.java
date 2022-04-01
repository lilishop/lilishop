package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.goods.entity.dos.StoreGoodsLabel;
import cn.lili.modules.goods.entity.vos.StoreGoodsLabelVO;
import cn.lili.modules.goods.mapper.StoreGoodsLabelMapper;
import cn.lili.modules.goods.service.StoreGoodsLabelService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * 店铺商品分类业务层实现
 *
 * @author Bulbasaur
 * @since 2020-03-07 16:18:56
 */
@Service
public class StoreGoodsLabelServiceImpl extends ServiceImpl<StoreGoodsLabelMapper, StoreGoodsLabel> implements StoreGoodsLabelService {

    /**
     * 缓存
     */
    @Autowired
    private Cache cache;

    @Override
    public List<StoreGoodsLabelVO> listByStoreId(String storeId) {

        //从缓存中获取店铺分类
        if (cache.hasKey(CachePrefix.STORE_CATEGORY.getPrefix() + storeId)) {
            return (List<StoreGoodsLabelVO>) cache.get(CachePrefix.STORE_CATEGORY.getPrefix() + storeId);
        }

        List<StoreGoodsLabel> list = list(storeId);
        List<StoreGoodsLabelVO> storeGoodsLabelVOList = new ArrayList<>();

        //循环列表判断是否为顶级，如果为顶级获取下级数据
        list.stream()
                .filter(storeGoodsLabel -> storeGoodsLabel.getLevel() == 0)
                .forEach(storeGoodsLabel -> {
                    StoreGoodsLabelVO storeGoodsLabelVO = new StoreGoodsLabelVO(storeGoodsLabel.getId(), storeGoodsLabel.getLabelName(), storeGoodsLabel.getLevel(), storeGoodsLabel.getSortOrder());
                    List<StoreGoodsLabelVO> storeGoodsLabelVOChildList = new ArrayList<>();
                    list.stream()
                            .filter(label -> label.getParentId().equals(storeGoodsLabel.getId()))
                            .forEach(storeGoodsLabelChild -> storeGoodsLabelVOChildList.add(new StoreGoodsLabelVO(storeGoodsLabelChild.getId(), storeGoodsLabelChild.getLabelName(), storeGoodsLabelChild.getLevel(), storeGoodsLabelChild.getSortOrder())));
                    storeGoodsLabelVO.setChildren(storeGoodsLabelVOChildList);
                    storeGoodsLabelVOList.add(storeGoodsLabelVO);
                });

        //调整店铺分类排序
        storeGoodsLabelVOList.sort(Comparator.comparing(StoreGoodsLabelVO::getSortOrder));

        if (!storeGoodsLabelVOList.isEmpty()) {
            cache.put(CachePrefix.CATEGORY.getPrefix() + storeId, storeGoodsLabelVOList);
        }
        return storeGoodsLabelVOList;
    }

    /**
     * 根据分类id集合获取所有店铺分类根据层级排序
     *
     * @param ids 商家ID
     * @return 店铺分类列表
     */
    @Override
    public List<StoreGoodsLabel> listByStoreIds(List<String> ids) {
        return this.list(new LambdaQueryWrapper<StoreGoodsLabel>().in(StoreGoodsLabel::getId, ids).orderByAsc(StoreGoodsLabel::getLevel));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public StoreGoodsLabel addStoreGoodsLabel(StoreGoodsLabel storeGoodsLabel) {
        //获取当前登录商家账号
        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser == null || CharSequenceUtil.isEmpty(tokenUser.getStoreId())) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        storeGoodsLabel.setStoreId(tokenUser.getStoreId());
        //保存店铺分类
        this.save(storeGoodsLabel);
        //清除缓存
        removeCache(storeGoodsLabel.getStoreId());
        return storeGoodsLabel;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public StoreGoodsLabel editStoreGoodsLabel(StoreGoodsLabel storeGoodsLabel) {
        //修改当前店铺的商品分类
        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser == null || CharSequenceUtil.isEmpty(tokenUser.getStoreId())) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        LambdaUpdateWrapper<StoreGoodsLabel> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.eq(StoreGoodsLabel::getStoreId, tokenUser.getStoreId());
        lambdaUpdateWrapper.eq(StoreGoodsLabel::getId, storeGoodsLabel.getId());
        //修改店铺分类
        this.update(storeGoodsLabel, lambdaUpdateWrapper);
        //清除缓存
        removeCache(storeGoodsLabel.getStoreId());
        return storeGoodsLabel;
    }

    @Override
    public void removeStoreGoodsLabel(String storeLabelId) {

        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser == null || CharSequenceUtil.isEmpty(tokenUser.getStoreId())) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        //删除店铺分类
        this.removeById(storeLabelId);

        //清除缓存
        removeCache(tokenUser.getStoreId());
    }

    /**
     * 获取店铺商品分类列表
     *
     * @param storeId 店铺ID
     * @return 店铺商品分类列表
     */
    private List<StoreGoodsLabel> list(String storeId) {
        LambdaQueryWrapper<StoreGoodsLabel> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(StoreGoodsLabel::getStoreId, storeId);
        queryWrapper.orderByDesc(StoreGoodsLabel::getSortOrder);
        return this.baseMapper.selectList(queryWrapper);
    }

    /**
     * 清除缓存
     */
    private void removeCache(String storeId) {
        cache.remove(CachePrefix.STORE_CATEGORY.getPrefix() + storeId);
    }
}
