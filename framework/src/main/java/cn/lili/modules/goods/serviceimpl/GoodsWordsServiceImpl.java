package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.GoodsWords;
import cn.lili.modules.goods.mapper.GoodsWordsMapper;
import cn.lili.modules.goods.service.GoodsWordsService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * 商品关键字业务层实现
 *
 * @author paulG
 * @since 2020/10/15
 **/
@Service
public class GoodsWordsServiceImpl extends ServiceImpl<GoodsWordsMapper, GoodsWords> implements GoodsWordsService {
}
