package cn.lili.modules.search.serviceimpl;

import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.search.entity.dos.CustomWords;
import cn.lili.modules.search.entity.vo.CustomWordsVO;
import cn.lili.modules.search.mapper.CustomWordsMapper;
import cn.lili.modules.search.service.CustomWordsService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.servlet.http.HttpServletResponse;
import java.nio.charset.StandardCharsets;
import java.util.List;

/**
 * 自定义分词业务层实现
 * @author paulG
 * @since 2020/10/15
 **/
@Service
public class CustomWordsServiceImpl extends ServiceImpl<CustomWordsMapper, CustomWords> implements CustomWordsService {

    @Override
    public String deploy() {
        LambdaQueryWrapper<CustomWords> queryWrapper = new LambdaQueryWrapper<CustomWords>().eq(CustomWords::getDisabled, 1);
        List<CustomWords> list = list(queryWrapper);

        HttpServletResponse response = ThreadContextHolder.getHttpResponse();
        StringBuilder builder = new StringBuilder();
        if (list != null && !list.isEmpty()) {
            boolean flag = true;
            for (CustomWords customWords : list) {
                if (flag) {
                    try {
                        response.setHeader("Last-Modified", customWords.getCreateTime().toString());
                        response.setHeader("ETag", Integer.toString(list.size()));
                    } catch (Exception e) {
                        log.error("自定义分词错误",e);
                    }
                    builder.append(customWords.getName());
                    flag = false;
                } else {
                    builder.append("\n");
                    builder.append(customWords.getName());
                }
            }
        }

        return new String(builder.toString().getBytes(StandardCharsets.UTF_8));
    }

    /**
     * 添加自定义分词
     *
     * @param customWordsVO 自定义分词信息
     * @return 是否添加成功
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean addCustomWords(CustomWordsVO customWordsVO) {
        LambdaQueryWrapper<CustomWords> queryWrapper = new LambdaQueryWrapper<CustomWords>().eq(CustomWords::getName, customWordsVO.getName());
        CustomWords one = this.getOne(queryWrapper, false);
        if (one != null && one.getDisabled().equals(1)) {
            return false;
        } else if (one != null && !one.getDisabled().equals(1)) {
            this.remove(queryWrapper);
        }
        customWordsVO.setDisabled(1);
        return this.save(customWordsVO);
    }

    /**
     * 删除自定义分词
     *
     * @param id 自定义分词id
     * @return 是否删除成功
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean deleteCustomWords(String id) {
        if (this.getById(id) == null) {
            throw new ServiceException(ResultCode.CUSTOM_WORDS_NOT_EXIST_ERROR);
        }
        return this.removeById(id);
    }

    /**
     * 修改自定义分词
     *
     * @param customWordsVO 自定义分词信息
     * @return 是否修改成功
     */
    @Override
    public boolean updateCustomWords(CustomWordsVO customWordsVO) {
        if (this.getById(customWordsVO.getId()) == null) {
            throw new ServiceException(ResultCode.CUSTOM_WORDS_NOT_EXIST_ERROR);
        }
        return this.updateById(customWordsVO);
    }

    /**
     * 分页查询自定义分词
     *
     * @param words  分词
     * @param pageVo 分页信息
     * @return 自定义分词分页信息
     */
    @Override
    public IPage<CustomWords> getCustomWordsByPage(String words, PageVO pageVo) {
        LambdaQueryWrapper<CustomWords> queryWrapper = new LambdaQueryWrapper<CustomWords>().like(CustomWords::getName, words);
        return this.page(PageUtil.initPage(pageVo), queryWrapper);
    }

    @Override
    public boolean existWords(String words) {
        LambdaQueryWrapper<CustomWords> queryWrapper = new LambdaQueryWrapper<CustomWords>().eq(CustomWords::getName, words);
        long count = count(queryWrapper);
        return count > 0;
    }
}
