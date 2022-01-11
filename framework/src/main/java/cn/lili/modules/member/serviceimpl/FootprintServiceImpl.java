package cn.lili.modules.member.serviceimpl;

import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.FootPrint;
import cn.lili.modules.member.mapper.FootprintMapper;
import cn.lili.modules.member.service.FootprintService;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.service.EsGoodsSearchService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * 会员浏览历史业务层实现
 *
 * @author Chopper
 * @since 2020/11/18 10:46 上午
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class FootprintServiceImpl extends ServiceImpl<FootprintMapper, FootPrint> implements FootprintService {

    /**
     * es商品业务层
     */
    @Autowired
    private EsGoodsSearchService esGoodsSearchService;

    @Override
    public FootPrint saveFootprint(FootPrint footPrint) {
        LambdaQueryWrapper<FootPrint> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(FootPrint::getMemberId, footPrint.getMemberId());
        queryWrapper.eq(FootPrint::getGoodsId, footPrint.getGoodsId());
        //如果已存在某商品记录，则更新其修改时间
        //如果不存在则添加记录
        List<FootPrint> oldPrints = list(queryWrapper);
        if (oldPrints != null && !oldPrints.isEmpty()) {
            FootPrint oldPrint = oldPrints.get(0);
            oldPrint.setSkuId(footPrint.getSkuId());
            this.updateById(oldPrint);
            return oldPrint;
        } else {
            footPrint.setCreateTime(new Date());
            this.save(footPrint);
            //删除超过100条后的记录
            this.baseMapper.deleteLastFootPrint(footPrint.getMemberId());
            return footPrint;
        }
    }

    @Override
    public boolean clean() {
        LambdaQueryWrapper<FootPrint> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(FootPrint::getMemberId, UserContext.getCurrentUser().getId());
        return this.remove(lambdaQueryWrapper);
    }

    @Override
    public boolean deleteByIds(List<String> ids) {
        LambdaQueryWrapper<FootPrint> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(FootPrint::getMemberId, UserContext.getCurrentUser().getId());
        lambdaQueryWrapper.in(FootPrint::getGoodsId, ids);
        this.remove(lambdaQueryWrapper);
        return true;
    }

    @Override
    public List<EsGoodsIndex> footPrintPage(PageVO pageVO) {

        LambdaQueryWrapper<FootPrint> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(FootPrint::getMemberId, UserContext.getCurrentUser().getId());
        lambdaQueryWrapper.eq(FootPrint::getDeleteFlag, false);
        lambdaQueryWrapper.orderByDesc(FootPrint::getUpdateTime);
        List<String> skuIdList = this.baseMapper.footprintSkuIdList(PageUtil.initPage(pageVO), lambdaQueryWrapper);
        if (!skuIdList.isEmpty()) {
            List<EsGoodsIndex> list = esGoodsSearchService.getEsGoodsBySkuIds(skuIdList);
            //去除为空的商品数据
            list.removeIf(Objects::isNull);
            return list;
        }
        return Collections.emptyList();
    }

    @Override
    public long getFootprintNum() {
        LambdaQueryWrapper<FootPrint> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(FootPrint::getMemberId, Objects.requireNonNull(UserContext.getCurrentUser()).getId());
        lambdaQueryWrapper.eq(FootPrint::getDeleteFlag, false);
        return this.count(lambdaQueryWrapper);
    }
}