package cn.lili.modules.im.service;


import cn.lili.common.vo.PageVO;
import cn.lili.modules.im.entity.dos.QA;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 问答
 *
 * @author pikachu
 * @since 2020-02-18 16:18:56
 */
public interface QAService extends IService<QA> {

    /**
     * 查询店铺问题
     * @param word
     * @param pageVO
     * @return
     */
    IPage<QA> getStoreQA(String word, PageVO pageVO);

}