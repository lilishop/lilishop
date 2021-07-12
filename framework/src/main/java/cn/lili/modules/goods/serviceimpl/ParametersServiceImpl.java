package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.Parameters;
import cn.lili.modules.goods.mapper.ParametersMapper;
import cn.lili.modules.goods.service.ParametersService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 商品参数业务层实现
 *
 * @author pikachu
 * @date 2020-03-02 16:18:56
 */
@Service
public class ParametersServiceImpl extends ServiceImpl<ParametersMapper, Parameters> implements ParametersService {
}