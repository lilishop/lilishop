package cn.lili.modules.store.serviceimpl;

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
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
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

    @Autowired
    private StoreGoodsLabelMapper storeGoodsLabelMapper;

    @Override
    public List<StoreGoodsLabelVO> listByStoreId(String storeId) {
        //TODO 从缓存获取店铺商品分类列表
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
        return storeGoodsLabelVOList;
    }

    @Override
    public List<StoreGoodsLabel> listByStore() {
        //获取当前登录商家账号
        AuthUser tokenUser = UserContext.getCurrentUser();
        //返回列表
        return list(tokenUser.getId());
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
        return storeGoodsLabelMapper.selectList(queryWrapper);
    }

    @Override
    public StoreGoodsLabel addStoreGoodsLabel(StoreGoodsLabel storeGoodsLabel) {
        //获取当前登录商家账号
        AuthUser tokenUser = UserContext.getCurrentUser();
        storeGoodsLabel.setStoreId(tokenUser.getStoreId());
        storeGoodsLabelMapper.insert(storeGoodsLabel);
        return storeGoodsLabel;
    }

    @Override
    public StoreGoodsLabel editStoreGoodsLabel(StoreGoodsLabel storeGoodsLabel) {
        //修改当前店铺的商品分类
        AuthUser tokenUser = UserContext.getCurrentUser();
        LambdaUpdateWrapper<StoreGoodsLabel> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.eq(StoreGoodsLabel::getStoreId, tokenUser.getStoreId());
        lambdaUpdateWrapper.eq(StoreGoodsLabel::getId, storeGoodsLabel.getId());
        storeGoodsLabelMapper.update(storeGoodsLabel, lambdaUpdateWrapper);
        return storeGoodsLabel;
    }

    @Override
    public void removeStoreGoodsLabel(String storeLabelId) {
        storeGoodsLabelMapper.deleteById(storeLabelId);
    }
}
