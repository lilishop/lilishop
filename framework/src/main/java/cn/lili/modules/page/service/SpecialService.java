package cn.lili.modules.page.service;

import cn.lili.modules.page.entity.dos.Special;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 专题活动业务层
 *
 * @author Bulbasaur
 * @since 2020/12/7 11:27
 */
public interface SpecialService extends IService<Special> {

    /**
     * 添加专题活动
     * @param special 专题活动
     * @return 专题活动
     */
    Special addSpecial(Special special);

    /**
     * 删除专题活动
     * @param id 活动ID
     * @return 操作状态
     */
    boolean removeSpecial(String id);

}
