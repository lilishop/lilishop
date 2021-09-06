package cn.lili.modules.goods.service;

import cn.lili.modules.goods.entity.dos.Parameters;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 商品参数业务层
 *
 * @author pikachu
 * @since 2020-03-02 16:18:56
 */
public interface ParametersService extends IService<Parameters> {



    /**
     * 更新参数组信息
     *
     * @param parameters 参数组信息
     * @return 是否更新成功
     */
    boolean updateParameter(Parameters parameters);

}