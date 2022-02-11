package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.GoodsUnit;
import cn.lili.modules.goods.mapper.GoodsUnitMapper;
import cn.lili.modules.goods.service.GoodsUnitService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


/**
 * 计量单位业务层实现
 *
 * @author Bulbasaur
 * @since 2020/11/26 16:13
 */
@Service
public class GoodsUnitServiceImpl extends ServiceImpl<GoodsUnitMapper, GoodsUnit> implements GoodsUnitService {

}