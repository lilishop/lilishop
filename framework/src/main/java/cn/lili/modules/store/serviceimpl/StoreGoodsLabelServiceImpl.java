package cn.lili.modules.store.serviceimpl;

import cn.lili.common.cache.Cache;
import cn.lili.common.cache.CachePrefix;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.store.entity.dos.StoreGoodsLabel;
import cn.lili.modules.store.entity.vos.StoreGoodsLabelVO;
import cn.lili.modules.store.mapper.StoreGoodsLabelMapper;
import cn.lili.modules.store.service.StoreGoodsLabelService;
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
 * @date 2020-03-07 16:18:56
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StoreGoodsLabelServiceImpl extends ServiceImpl<StoreGoodsLabelMapper, StoreGoodsLabel> implements StoreGoodsLabelService {

    /**
     * 缓存
     */
    @Autowired
    private Cache cache;

    @Override
    public List<StoreGoodsLabelVO> listByStoreId(String storeId) {

        //从缓存中获取店铺分类
        if (cache.hasKey(CachePrefix.STORE_CATEGORY.getPrefix() + storeId + "tree")) {
            return (List<StoreGoodsLabelVO>) cache.get(CachePrefix.CATEGORY.getPrefix() + "tree");
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
        storeGoodsLabelVOList.sort(new Comparator<StoreGoodsLabelVO>() {
            @Override
            public int compare(StoreGoodsLabelVO o1, StoreGoodsLabelVO o2) {
                return o1.getSortOrder().compareTo(o2.getSortOrder());
            }
        });

        if (storeGoodsLabelVOList.size() != 0) {
            cache.put(CachePrefix.CATEGORY.getPrefix() + storeId + "tree", storeGoodsLabelVOList);
        }
        return storeGoodsLabelVOList;
    }

    @Override
    public StoreGoodsLabel addStoreGoodsLabel(StoreGoodsLabel storeGoodsLabel) {
        //获取当前登录商家账号
        AuthUser tokenUser = UserContext.getCurrentUser();
        storeGoodsLabel.setStoreId(tokenUser.getStoreId());
        //保存店铺分类
        this.save(storeGoodsLabel);
        //清除缓存
        removeCache(storeGoodsLabel.getStoreId());
        return storeGoodsLabel;
    }

    @Override
    public StoreGoodsLabel editStoreGoodsLabel(StoreGoodsLabel storeGoodsLabel) {
        //修改当前店铺的商品分类
        AuthUser tokenUser = UserContext.getCurrentUser();
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

        //删除店铺分类
        this.removeById(storeLabelId);

        //清除缓存
        removeCache(UserContext.getCurrentUser().getStoreId());
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
        return this.baseMapper.selectList(queryWrapper);
    }

    /**
     * 清除缓存
     */
    private void removeCache(String storeId) {
        cache.remove(CachePrefix.CATEGORY.getPrefix() + storeId + "tree");
    }
}
