package cn.lili.modules.message.service;

import cn.lili.modules.message.entity.dos.ShortLink;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 短链接 业务层
 *
 * @author Chopper
 */
public interface ShortLinkService extends IService<ShortLink> {

    /**
     * 根据模型，查询返回的集合
     *
     * @param shortLink 短链接模型
     * @return 端链接集合
     */
    List<ShortLink> queryShortLinks(ShortLink shortLink);
}