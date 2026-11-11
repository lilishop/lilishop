package cn.lili.modules.im.mapper;

import cn.lili.modules.im.entity.dos.ImImage;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;

/**
 * 客服图片上传凭据持久化，支持按会话及上传者核验图片来源。
 * @author Bulbasaur
 * @since 2026-11-09
 */
@Mapper
public interface ImImageMapper extends BaseMapper<ImImage> {
}
