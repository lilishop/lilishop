package cn.lili.modules.system.service;

import cn.lili.modules.system.entity.dos.SensitiveWords;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 敏感词业务层
 *
 * @author Bulbasaur
 * @since 2020/11/17 8:02 下午
 */
public interface SensitiveWordsService extends IService<SensitiveWords> {

    /**
     * 重新写入缓存
     */
    void resetCache();

}