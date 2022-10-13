package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.collection.CollUtil;
import cn.lili.cache.Cache;
import cn.lili.modules.goods.entity.dos.Wholesale;
import cn.lili.modules.goods.mapper.WholesaleMapper;
import cn.lili.modules.goods.service.WholesaleService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author paulG
 * @since 2022/5/24
 **/
@Service
@CacheConfig(cacheNames = "{wholesale}_")
public class WholesaleServiceImpl extends ServiceImpl<WholesaleMapper, Wholesale> implements WholesaleService {

    @Autowired
    private Cache<List<Wholesale>> cache;

    @Override
    @Cacheable(key = "#goodsId")
    public List<Wholesale> findByGoodsId(String goodsId) {
        LambdaQueryWrapper<Wholesale> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Wholesale::getGoodsId, goodsId);
        return this.list(queryWrapper).stream().sorted(Comparator.comparing(Wholesale::getNum)).collect(Collectors.toList());
    }

    @Override
    @Cacheable(key = "#templateId+'_template'")
    public List<Wholesale> findByTemplateId(String templateId) {
        LambdaQueryWrapper<Wholesale> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Wholesale::getTemplateId, templateId);
        return this.list(queryWrapper).stream().sorted(Comparator.comparing(Wholesale::getNum)).collect(Collectors.toList());
    }

    @Override
    @CacheEvict(key = "#templateId+'_template'")
    public Boolean removeByTemplateId(String templateId) {
        LambdaQueryWrapper<Wholesale> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Wholesale::getTemplateId, templateId);
        cache.remove("{wholesale}_" + templateId + "_template");
        return this.remove(queryWrapper);
    }

    @Override
    @CacheEvict(key = "#goodsId")
    public Boolean removeByGoodsId(String goodsId) {
        LambdaQueryWrapper<Wholesale> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Wholesale::getGoodsId, goodsId);
        cache.remove("{wholesale}_" + goodsId);
        return this.remove(queryWrapper);
    }

    @Override
    public Wholesale match(String goodsId, Integer num) {
        List<Wholesale> wholesaleList = cache.get("{wholesale}_" + goodsId);
        if (wholesaleList == null) {
            wholesaleList = this.findByGoodsId(goodsId);
            cache.put("{wholesale}_" + goodsId, wholesaleList);
        }
        List<Wholesale> matchList = wholesaleList.stream()
                .filter(wholesale -> wholesale.getNum() <= num)
                .collect(Collectors.toList());
        if (CollUtil.isNotEmpty(matchList)) {
            return matchList.get(matchList.size() - 1);
        } else if (CollUtil.isNotEmpty(wholesaleList) && CollUtil.isEmpty(matchList)) {
            return wholesaleList.get(0);
        }
        return null;
    }
}
