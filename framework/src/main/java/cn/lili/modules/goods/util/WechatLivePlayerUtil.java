package cn.lili.modules.goods.util;

import cn.hutool.json.JSONObject;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.HttpUtils;
import cn.lili.modules.goods.entity.dos.Commodity;
import cn.lili.modules.goods.entity.dos.Studio;
import cn.lili.modules.goods.entity.dto.GoodsInfo;
import cn.lili.modules.wechat.util.WechatAccessTokenUtil;
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
 * @since 2021/5/17 10:16 上午
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
    public Map<String, String> create(Studio studio) {
        //发送url
        String url = "https://api.weixin.qq.com/wxaapi/broadcast/room/create?access_token=";
        //添加直播间
        Map<String, String> map = this.mockRoom(studio);
        JSONObject json = this.doPostWithJson(url, map);
        Map<String, String> roomMap = new HashMap<>(2);
        roomMap.put("roomId", json.getStr("roomId"));
        roomMap.put("qrcodeUrl", json.getStr("qrcode_url"));
        return roomMap;
    }

    /**
     * 创建小程序直播间
     *
     * @param studio 小程序直播
     * @return 房间ID
     */
    public boolean editRoom(Studio studio) {
        //发送url
        String url = "https://api.weixin.qq.com/wxaapi/broadcast/room/editroom?access_token=";

        //修改直播间
        Map<String, String> map = this.mockRoom(studio);
        map.put("id", studio.getRoomId().toString());
        this.doPostWithJson(url, map);
        return true;
    }


    /**
     * 获取直播间回放
     *
     * @param roomId 房间ID
     * @return 回放地址
     */
    public String getLiveInfo(Integer roomId) {
        //发送url
        String url = "https://api.weixin.qq.com/wxa/business/getliveinfo?access_token=";
        Map<String, Object> map = new HashMap<>(16);
        //获取回放
        map.put("action", "get_replay");
        //直播间ID
        map.put("room_id", roomId);
        //起始拉取视频，0表示从第一个视频片段开始拉取
        map.put("start", "0");
        //每次拉取的数量，建议100以内
        map.put("limit", "1");
        JSONObject json = this.doPostWithJson(url, map);
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
        //发送url
        String url = "https://api.weixin.qq.com/wxaapi/broadcast/room/addgoods?access_token=";
        Map<String, Object> map = new HashMap<>(16);
        //直播间回放
        Integer[] ids = {goodsId};
        map.put("ids", ids);
        //商品ID
        map.put("roomId", roomId);
        this.doPostWithJson(url, map);
        return true;
    }

    /**
     * 删除直播间商品
     *
     * @param roomId  房间ID
     * @param goodsId 商品ID
     * @return 操作结果
     */
    public Boolean goodsDeleteInRoom(Integer roomId, Integer goodsId) {
        //发送url
        String url = "https://api.weixin.qq.com/wxaapi/broadcast/goods/deleteInRoom?access_token=";
        Map<String, Integer> map = new HashMap<>(2);
        //直播间回放
        map.put("goodsId", goodsId);
        //商品ID
        map.put("roomId", roomId);
        this.doPostWithJson(url, map);
        return true;

    }

    /**
     * 添加直播商品
     *
     * @param commodity 直播商品
     * @return 添加结果
     */
    public JSONObject addGoods(Commodity commodity) {
        //发送url
        String url = "https://api.weixin.qq.com/wxaapi/broadcast/goods/add?access_token=";

        //商品名称，最长14个汉字
        if (commodity.getName().length() > 14) {
            commodity.setName(commodity.getName().substring(0, 13));
        }

        //新建微信商品DTO
        GoodsInfo goodsInfo = new GoodsInfo(commodity);
        //上传微信临时图片
        goodsInfo.setCoverImgUrl(wechatMediaUtil.uploadMedia("image", commodity.getGoodsImage()));
        Map<String, GoodsInfo> map = new HashMap<>(2);
        //调用新增直播商品接口
        map.put("goodsInfo", goodsInfo);
        return this.doPostWithJson(url, map);
    }

    /**
     * 删除直播商品
     *
     * @param goodsId 商品ID
     * @return 删除结果
     */
    public JSONObject deleteGoods(String goodsId) {
        //发送url
        String url = "https://api.weixin.qq.com/wxaapi/broadcast/goods/delete?access_token=";
        Map<String, Object> map = new HashMap<>(2);
        map.put("goodsId", goodsId);
        return this.doPostWithJson(url, map);
    }

    /**
     * 查询直播商品状态
     *
     * @param goodsIdList 商品ID列表
     * @return 删除结果
     */
    public JSONObject getGoodsWareHouse(List<String> goodsIdList) {
        //发送url
        String url = "https://api.weixin.qq.com/wxa/business/getgoodswarehouse?access_token=";
        Map<String, Object> map = new HashMap<>(2);
        map.put("goods_ids", goodsIdList);
        return this.doPostWithJson(url, map);
    }

    /**
     * 请求微信接口
     *
     * @param url 链接
     * @param map 参数
     * @return 返回内容
     */
    private JSONObject doPostWithJson(String url, Map map) {
        //获取token
        String token = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.WECHAT_MP);
        //请求链接添加token
        url += token;
        //发起请求
        String content = HttpUtils.doPostWithJson(url, map);
        //记录请求结果
        log.info("微信小程序请求结果：" + content);
        //获取请求内容，如果token过期则重新获取，如果出错则抛出错误
        JSONObject jsonObject = new JSONObject(content);
        if (("0").equals(jsonObject.get("errcode").toString())) {
            return jsonObject;
        } else if (("40001").equals(jsonObject.get("errcode"))) {
            wechatAccessTokenUtil.removeAccessToken(ClientTypeEnum.WECHAT_MP);
            return this.doPostWithJson(url, map);
        } else {
            throw new ServiceException(jsonObject.get("errmsg").toString());
        }
    }

    private Map<String, String> mockRoom(Studio studio) {
        Map<String, String> map = new HashMap<>(16);
        //背景图
        map.put("coverImg", wechatMediaUtil.uploadMedia("image", studio.getCoverImg()));
        //分享图
        map.put("shareImg", wechatMediaUtil.uploadMedia("image", studio.getShareImg()));
        //购物直播频道封面图
        map.put("feedsImg", wechatMediaUtil.uploadMedia("image", studio.getFeedsImg()));
        //直播间名字
        map.put("name", studio.getName());
        //直播计划开始时间
        map.put("startTime", studio.getStartTime());
        //直播计划结束时间
        map.put("endTime", studio.getEndTime());
        //主播昵称
        map.put("anchorName", studio.getAnchorName());
        //主播微信号
        map.put("anchorWechat", studio.getAnchorWechat());
        //直播间类型
        map.put("type", "0");
        //是否关闭点赞
        map.put("closeLike", "0");
        //是否关闭货架
        map.put("closeGoods", "0");
        //是否关闭评论
        map.put("closeComment", "0");
        //直播间名字
        map.put("closeReplay", "0");

        return map;
    }
}
