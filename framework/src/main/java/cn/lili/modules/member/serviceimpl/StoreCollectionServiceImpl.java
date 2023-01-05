package cn.lili.modules.member.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.StoreCollection;
import cn.lili.modules.member.entity.dto.CollectionDTO;
import cn.lili.modules.member.entity.vo.StoreCollectionVO;
import cn.lili.modules.member.mapper.StoreCollectionMapper;
import cn.lili.modules.member.service.StoreCollectionService;
import cn.lili.modules.store.service.StoreService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

/**
 * 会员店铺收藏业务层实现
 *
 * @author Chopper
 * @since 2020/11/18 2:52 下午
 */
@Service
public class StoreCollectionServiceImpl extends ServiceImpl<StoreCollectionMapper, StoreCollection> implements StoreCollectionService {


    @Autowired
    private StoreService storeService;

    @Override
    public IPage<StoreCollectionVO> storeCollection(PageVO pageVo) {
        QueryWrapper<StoreCollectionVO> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("sc.member_id", UserContext.getCurrentUser().getId());
        queryWrapper.orderByDesc("sc.create_time");
        return this.baseMapper.storeCollectionVOList(PageUtil.initPage(pageVo), queryWrapper);
    }

    @Override
    public boolean isCollection(String storeId) {
        QueryWrapper<StoreCollection> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("member_id", UserContext.getCurrentUser().getId());
        queryWrapper.eq("store_id", storeId);
        return Optional.ofNullable(this.getOne(queryWrapper, false)).isPresent();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public StoreCollection addStoreCollection(String storeId) {
        if (this.getOne(new LambdaUpdateWrapper<StoreCollection>()
                .eq(StoreCollection::getMemberId, UserContext.getCurrentUser().getId())
                .eq(StoreCollection::getStoreId, storeId)) == null) {
            StoreCollection storeCollection = new StoreCollection(UserContext.getCurrentUser().getId(), storeId);
            this.save(storeCollection);
            storeService.updateStoreCollectionNum(new CollectionDTO(storeId, 1));
            return storeCollection;
        }
        throw new ServiceException(ResultCode.USER_COLLECTION_EXIST);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean deleteStoreCollection(String storeId) {
        QueryWrapper<StoreCollection> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("member_id", UserContext.getCurrentUser().getId());
        queryWrapper.eq("store_id", storeId);
        storeService.updateStoreCollectionNum(new CollectionDTO(storeId, -1));
        return this.remove(queryWrapper);
    }
}