package cn.lili.modules.broadcast.serviceimpl;

import cn.lili.common.security.context.UserContext;
import cn.lili.modules.broadcast.entity.dos.Studio;
import cn.lili.modules.broadcast.mapper.StudioMapper;
import cn.lili.modules.broadcast.service.StudioService;
import cn.lili.modules.broadcast.util.WechatLivePlayerUtil;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * 小程序直播间业务层实现
 *
 * @author Bulbasaur
 * @date: 2021/5/17 10:04 上午
 */
public class StudioServiceImpl extends ServiceImpl<StudioMapper, Studio>  implements StudioService {

    @Autowired
    private WechatLivePlayerUtil wechatLivePlayerUtil;

    @Override
    public Boolean create(Studio studio) {

        //创建小程序直播
        String roomId=wechatLivePlayerUtil.create(studio);
        studio.setRoomId(roomId);
        studio.setStoreId(UserContext.getCurrentUser().getStoreId());
        return this.save(studio);
    }

    @Override
    public String getLiveInfo(String roomId) {
        Studio studio=this.getByRoomId(roomId);
        //获取直播间并判断回放内容是否为空，如果为空则获取直播间回放并保存
        if(studio.getMediaUrl()!=null){
            return studio.getMediaUrl();
        }else{
            String mediaUrl= wechatLivePlayerUtil.getLiveInfo(roomId);
            studio.setMediaUrl(mediaUrl);
            this.save(studio);
            return mediaUrl;
        }
    }

    private Studio getByRoomId(String roomId){
        return this.getOne(this.lambdaQuery().eq(Studio::getRoomId,roomId)) ;
    }
}
