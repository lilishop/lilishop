package cn.lili.modules.search.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.search.entity.dos.CustomWords;
import cn.lili.modules.search.entity.vo.CustomWordsVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 自定义分词业务层
 *
 * @author paulG
 * @since 2020/10/15
 **/
public interface CustomWordsService extends IService<CustomWords> {

    /**
     * 自定义分词部署替换
     * @return 替换的内容
     */
    String deploy();

    /**
     * 是否存在分词
     * @param words 分词
     * @return 是否存在
     */
    boolean existWords(String words);

    /**
     * 添加自定义分词
     *
     * @param customWordsVO 自定义分词信息
     * @return 是否添加成功
     */
    boolean addCustomWords(CustomWordsVO customWordsVO);


    /**
     * 修改自定义分词
     *
     * @param customWordsVO 自定义分词信息
     * @return 是否修改成功
     */
    boolean updateCustomWords(CustomWordsVO customWordsVO);

    /**
     * 删除自定义分词
     *
     * @param id 自定义分词id
     * @return 是否删除成功
     */
    boolean deleteCustomWords(String id);

    /**
     * 根据名字批量删除
     *
     * @param names 名称列表
     * @return 是否删除成功
     */
    boolean deleteBathByName(List<String> names);

    /**
     * 批量插入自定义分词
     *
     * @param customWordsList 自定义分词列表
     * @return 受影响行数
     */
    long insertBatchCustomWords(List<CustomWords> customWordsList);

    /**
     * 分页查询自定义分词
     *
     * @param words 分词
     * @param pageVo 分页信息
     * @return 自定义分词分页信息
     */
    IPage<CustomWords> getCustomWordsByPage(String words, PageVO pageVo);

}
