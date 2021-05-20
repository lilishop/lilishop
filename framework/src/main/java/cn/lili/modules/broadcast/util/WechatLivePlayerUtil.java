package cn.lili.modules.broadcast.util;

import cn.hutool.core.convert.Convert;
import cn.hutool.json.JSONObject;
import cn.lili.modules.base.entity.enums.ClientTypeEnum;
import cn.lili.modules.broadcast.entity.dos.Commodity;
import cn.lili.modules.broadcast.entity.dos.Studio;
import cn.lili.modules.broadcast.entity.dto.GoodsInfo;
import cn.lili.modules.message.util.WechatAccessTokenUtil;
import cn.lili.modules.system.utils.HttpUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 微信小程序直播工具类
 *
 * @author Bulbasaur
 * @date: 2021/5/17 10:16 上午
 */
@Slf4j
@Component
public class WechatLivePlayerUtil {

    @Autowired
    private WechatAccessTokenUtil wechatAccessTokenUtil;
    @Autowired
    private WechatMediaUtil wechatMediaUtil;

    /**
     * 创建小程序直播间
     *
     * @param studio 小程序直播
     * @return 房间ID
     */
    public Integer create(Studio studio) throws Exception{
        //获取token
        String token = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.WECHAT_MP);
        //发送url
        String url = "https://api.weixin.qq.com/wxaapi/broadcast/room/create?access_token=" + token;

        Map<String, String> map = new HashMap<>();
        // 背景图
        map.put("coverImg", wechatMediaUtil.uploadMedia(token,"image",studio.getCoverImg()));
        // 分享图
        map.put("shareImg", wechatMediaUtil.uploadMedia(token,"image",studio.getShareImg()));
        // 购物直播频道封面图
        map.put("feedsImg", wechatMediaUtil.uploadMedia(token,"image",studio.getFeedsImg()));
        // 直播间名字
        map.put("name", studio.getName());
        // 直播计划开始时间
        map.put("startTime", studio.getStartTime());
        // 直播计划结束时间
        map.put("endTime", studio.getEndTime());
        // 主播昵称
        map.put("anchorName", studio.getAnchorName());
        // 主播微信号
        map.put("anchorWechat", studio.getAnchorWechat());
        // 直播间类型
        map.put("type", "0");
        // 是否关闭点赞
        map.put("closeLike", "0");
        // 是否关闭货架
        map.put("closeGoods", "0");
        // 是否关闭评论
        map.put("closeComment", "0");
        // 直播间名字
        map.put("closeReplay", "0");

        String content = HttpUtils.doPostWithJson(url, map);
        JSONObject json = new JSONObject(content);
        log.info("微信小程序直播间创建结果：" + content);
        return Convert.toInt(json.getStr("roomId"));
    }

    /**
     * 获取直播间回放
     *
     * @param roomId 房间ID
     * @return 回放地址
     */
    public String getLiveInfo(Integer roomId) {
        //获取token
        String token = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.WECHAT_MP);
        //发送url
        String url = "https://api.weixin.qq.com/wxa/business/getliveinfo?access_token=" + token;
        Map<String, Object> map = new HashMap<>();
        // 获取回放
        map.put("action", "get_replay");
        // 直播间ID
        map.put("room_id", roomId);
        // 起始拉取视频，0表示从第一个视频片段开始拉取
        map.put("start", "0");
        // 每次拉取的数量，建议100以内
        map.put("limit", "1");

        String content = HttpUtils.doPostWithJson(url, map);
        JSONObject json = new JSONObject(content);
        log.info("微信小程序获取信息：" + content);
        //TODO get media_url
        return json.getStr("live_replay");
    }

    /**
     * 推送直播间商品
     *
     * @param roomId  房间ID
     * @param goodsId 商品ID
     * @return 操作结果
     */
    public Boolean pushGoods(Integer roomId, Integer goodsId) {
        //获取token
        String token = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.WECHAT_MP);
        //发送url
        String url = "https://api.weixin.qq.com/wxaapi/broadcast/goods/push?access_token=" + token;
        Map<String, Integer> map = new HashMap<>();
        // 直播间回放
        map.put("goodsId", goodsId);
        // 商品ID
        map.put("roomId", roomId);
        String content = HttpUtils.doPostWithJson(url, map);
        JSONObject json = new JSONObject(content);
        log.info("微信小程序直播间推送商品：" + content);
        return json.getStr("errcode").equals("0");
    }

    /**
     * 删除直播间商品
     *
     * @param roomId  房间ID
     * @param goodsId 商品ID
     * @return 操作结果
     */
    public Boolean goodsDeleteInRoom(Integer roomId, Integer goodsId) {
        //获取token
        String token = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.WECHAT_MP);
        //发送url
        String url = "https://api.weixin.qq.com/wxaapi/broadcast/goods/deleteInRoom?access_token=" + token;
        Map<String, Integer> map = new HashMap<>();
        // 直播间回放
        map.put("goodsId", goodsId);
        // 商品ID
        map.put("roomId", roomId);
        String content = HttpUtils.doPostWithJson(url, map);
        JSONObject json = new JSONObject(content);
        log.info("微信小程序直播间删除商品：" + content);
        return json.getStr("errcode").equals("0");
    }

    /**
     * 添加直播商品
     *
     * @param commodity 直播商品
     * @return 添加结果
     */
    public JSONObject addGoods(Commodity commodity) {
        //获取token
        String token = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.WECHAT_MP);
        //发送url
        String url = "https://api.weixin.qq.com/wxaapi/broadcast/goods/add?access_token=" + token;
        //新建微信商品DTO
        GoodsInfo goodsInfo = new GoodsInfo(commodity);
        goodsInfo.setCoverImgUrl(wechatMediaUtil.uploadMedia(token,"image",commodity.getGoodsImage()));
        String content = HttpUtils.doPostWithJson(url, goodsInfo);
        JSONObject json = new JSONObject(content);
        log.info("微信小程序添加直播商品结果：" + content);
        return json;
    }

    /**
     * 删除直播商品
     *
     * @param goodsId 商品ID
     * @return 删除结果
     */
    public JSONObject deleteGoods(String goodsId) {
        //获取token
        String token = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.WECHAT_MP);
        //发送url
        String url = "https://api.weixin.qq.com/wxaapi/broadcast/goods/delete?access_token=" + token;
        String content = HttpUtils.doPostWithJson(url, goodsId);
        JSONObject json = new JSONObject(content);
        log.info("微信小程序删除直播商品结果：" + content);
        return json;
    }

    /**
     * 查询直播商品状态
     *
     * @param goodsIdList 商品ID列表
     * @return 删除结果
     */
    public JSONObject getGoodsWareHouse(List<String> goodsIdList) {
        //获取token
        String token = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.WECHAT_MP);
        //发送url
        String url = "https://api.weixin.qq.com/wxa/business/getgoodswarehouse?access_token=" + token;
        String content = HttpUtils.doPostWithJson(url, goodsIdList);
        JSONObject json = new JSONObject(content);
        log.info("微信小程序查询直播商品结果：" + content);
        return json;

    }
}
