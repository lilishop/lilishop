package cn.lili.modules.connect.serviceimpl;

import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.token.Token;
import cn.lili.common.utils.CookieUtil;
import cn.lili.common.utils.HttpUtils;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.entity.dto.ConnectAuthUser;
import cn.lili.modules.connect.entity.dto.WechatMPLoginParams;
import cn.lili.modules.connect.entity.enums.ConnectEnum;
import cn.lili.modules.connect.mapper.ConnectMapper;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dto.ConnectQueryDTO;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.member.token.MemberTokenGenerate;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.connect.WechatConnectSetting;
import cn.lili.modules.system.entity.dto.connect.dto.WechatConnectSettingItem;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import javax.naming.NoPermissionException;
import java.nio.charset.StandardCharsets;
import java.security.AlgorithmParameters;
import java.security.Security;
import java.util.*;
import java.util.concurrent.TimeUnit;

/**
 * 联合登陆接口实现
 *
 * @author Chopper
 */
@Slf4j
@Service
public class ConnectServiceImpl extends ServiceImpl<ConnectMapper, Connect> implements ConnectService {


    @Autowired
    private SettingService settingService;
    @Autowired
    private MemberService memberService;
    @Autowired
    private MemberTokenGenerate memberTokenGenerate;
    @Autowired
    private Cache cache;

    static boolean AUTO_REGION = true;


    @Override
    public Token unionLoginCallback(String type, String unionid, String uuid, boolean longTerm) throws NoPermissionException {

        try {
            LambdaQueryWrapper<Connect> queryWrapper = new LambdaQueryWrapper<>();
            queryWrapper.eq(Connect::getUnionId, unionid);
            queryWrapper.eq(Connect::getUnionType, type);
            //查询绑定关系
            Connect connect = this.getOne(queryWrapper);
            if (connect == null) {
                throw new NoPermissionException("未绑定用户");
            }
            //查询会员
            Member member = memberService.getById(connect.getUserId());
            //如果未绑定会员，则把刚才查询到的联合登录表数据删除
            if (member == null) {
                this.remove(queryWrapper);
                throw new NoPermissionException("未绑定用户");
            }
            return memberTokenGenerate.createToken(member, longTerm);
        } catch (NoPermissionException e) {
            throw e;
        }
    }

    @Override
    public Token unionLoginCallback(String type, ConnectAuthUser authUser, String uuid) {

        Token token;
        try {
            token = this.unionLoginCallback(type, authUser.getUuid(), uuid, false);
        } catch (NoPermissionException e) {
            if (AUTO_REGION) {
                token = memberService.autoRegister(authUser);
                return token;
            } else {
                //写入cookie
                CookieUtil.addCookie(CONNECT_COOKIE, uuid, 1800, ThreadContextHolder.getHttpResponse());
                CookieUtil.addCookie(CONNECT_TYPE, type, 1800, ThreadContextHolder.getHttpResponse());
                //自动登录失败，则把信息缓存起来
                cache.put(ConnectService.cacheKey(type, uuid), authUser, 30L, TimeUnit.MINUTES);
                throw new ServiceException(ResultCode.USER_NOT_BINDING);
            }
        } catch (Exception e) {
            log.error("联合登陆异常：", e);
            throw new ServiceException(ResultCode.ERROR);
        }
        return token;
    }

    @Override
    public void bind(String unionId, String type) {
        AuthUser authUser = UserContext.getCurrentUser();
        Connect connect = new Connect(authUser.getId(), unionId, type);
        this.save(connect);
    }

    @Override
    public void unbind(String type) {

        LambdaQueryWrapper<Connect> queryWrapper = new LambdaQueryWrapper<>();

        queryWrapper.eq(Connect::getUserId, UserContext.getCurrentUser().getId());
        queryWrapper.eq(Connect::getUnionType, type);

        this.remove(queryWrapper);
    }

    @Override
    public List<String> bindList() {
        LambdaQueryWrapper<Connect> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Connect::getUserId, UserContext.getCurrentUser().getId());
        List<Connect> connects = this.list(queryWrapper);
        List<String> keys = new ArrayList<>();
        connects.forEach(item -> {
            keys.add(item.getUnionType());
        });
        return keys;
    }

    @Override
    public Token appLoginCallback(ConnectAuthUser authUser, String uuid) {
        try {
            return this.unionLoginCallback(authUser.getSource(), authUser.getUuid(), uuid, true);
        } catch (NoPermissionException e) {
            return memberService.autoRegister(authUser);
        }
    }


    @Override
    public Token miniProgramAutoLogin(WechatMPLoginParams params) {

        Object cacheData = cache.get(CachePrefix.WECHAT_SESSION_PARAMS.getPrefix() + params.getUuid());
        Map<String, String> map = new HashMap<>(3);
        if (cacheData == null) {
            //得到微信小程序联合登陆信息
            JSONObject json = this.getConnect(params.getCode());
            //存储session key 后续登录用得到
            String sessionKey = json.getStr("session_key");
            String unionId = json.getStr("unionid");
            String openId = json.getStr("openid");
            map.put("sessionKey", sessionKey);
            map.put("unionId", unionId);
            map.put("openId", openId);
            cache.put(CachePrefix.WECHAT_SESSION_PARAMS.getPrefix() + params.getUuid(), map, 900L);
        } else {
            map = (Map<String, String>) cacheData;
        }
        //微信联合登陆参数

        return phoneMpBindAndLogin(map.get("sessionKey"), params, map.get("openId"), map.get("unionId"));
    }

    /**
     * 通过微信返回等code 获取openid 等信息
     *
     * @param code
     * @return
     */
    public JSONObject getConnect(String code) {
        WechatConnectSettingItem setting = getWechatMPSetting();
        String url = "https://api.weixin.qq.com/sns/jscode2session?" +
                "appid=" + setting.getAppId() + "&" +
                "secret=" + setting.getAppSecret() + "&" +
                "js_code=" + code + "&" +
                "grant_type=authorization_code";
        String content = HttpUtils.doGet(url, "UTF-8", 100, 1000);
        log.error(content);
        return JSONUtil.parseObj(content);
    }

    /**
     * 手机号 绑定 且 自动登录
     *
     * @param sessionKey 微信sessionKey
     * @param params     微信小程序自动登录参数
     * @param openId     微信openid
     * @param unionId    微信unionid
     * @return
     */
    public Token phoneMpBindAndLogin(String sessionKey, WechatMPLoginParams params, String openId, String unionId) {
        String encryptedData = params.getEncryptedData(), iv = params.getIv();
        JSONObject userInfo = this.getUserInfo(encryptedData, sessionKey, iv);
        log.info("联合登陆返回：{}", userInfo.toString());
        String phone = (String) userInfo.get("purePhoneNumber");

        //手机号登录
        LambdaQueryWrapper<Member> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.eq(Member::getMobile, phone);
        Member member = memberService.getOne(lambdaQueryWrapper);
        //如果不存在会员，则进行绑定微信openid 和 unionid，并且登录
        if (member != null) {
            bindMpMember(openId, unionId, member);
            return memberTokenGenerate.createToken(member, true);
        }

        //如果没有会员，则根据手机号注册会员
        Member newMember = new Member("m" + phone, "111111", phone, params.getNickName(), params.getImage());
        memberService.save(newMember);
        newMember = memberService.findByUsername(newMember.getUsername());
        bindMpMember(openId, unionId, newMember);
        return memberTokenGenerate.createToken(newMember, true);
    }

    @Override
    public Connect queryConnect(ConnectQueryDTO connectQueryDTO) {

        LambdaQueryWrapper<Connect> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(StringUtils.isNotEmpty(connectQueryDTO.getUserId()), Connect::getUserId, connectQueryDTO.getUserId())
                .eq(StringUtils.isNotEmpty(connectQueryDTO.getUnionType()), Connect::getUnionType, connectQueryDTO.getUnionType())
                .eq(StringUtils.isNotEmpty(connectQueryDTO.getUnionId()), Connect::getUnionId, connectQueryDTO.getUnionId());
        return this.getOne(queryWrapper);
    }

    /**
     * 会员绑定 绑定微信小程序
     * <p>
     * 如果openid 已经绑定其他账号，则这里不作处理，如果未绑定，则绑定最新的会员
     * 这样，微信小程序注册之后，其他app 公众号页面，都可以实现绑定自动登录功能
     * </p>
     *
     * @param openId
     * @param unionId
     * @param member
     */
    private void bindMpMember(String openId, String unionId, Member member) {


        //如果unionid 不为空  则为账号绑定unionid
        if (StringUtils.isNotEmpty(unionId)) {
            LambdaQueryWrapper<Connect> lambdaQueryWrapper = new LambdaQueryWrapper();
            lambdaQueryWrapper.eq(Connect::getUnionId, unionId);
            lambdaQueryWrapper.eq(Connect::getUnionType, ConnectEnum.WECHAT.name());
            List<Connect> connects = this.list(lambdaQueryWrapper);
            if (connects.size() == 0) {
                Connect connect = new Connect();
                connect.setUnionId(unionId);
                connect.setUserId(member.getId());
                connect.setUnionType(ConnectEnum.WECHAT.name());
                this.save(connect);
            }
        }//如果openid 不为空  则为账号绑定openid
        if (StringUtils.isNotEmpty(openId)) {
            LambdaQueryWrapper<Connect> lambdaQueryWrapper = new LambdaQueryWrapper();
            lambdaQueryWrapper.eq(Connect::getUnionId, openId);
            lambdaQueryWrapper.eq(Connect::getUnionType, ConnectEnum.WECHAT_MP_OPEN_ID.name());
            List<Connect> connects = this.list(lambdaQueryWrapper);
            if (connects.size() == 0) {
                Connect connect = new Connect();
                connect.setUnionId(openId);
                connect.setUserId(member.getId());
                connect.setUnionType(ConnectEnum.WECHAT_MP_OPEN_ID.name());
                this.save(connect);
            }
        }

    }

    /**
     * 获取微信小程序配置
     *
     * @return
     */
    private WechatConnectSettingItem getWechatMPSetting() {
        Setting setting = settingService.get(SettingEnum.WECHAT_CONNECT.name());

        WechatConnectSetting wechatConnectSetting = JSONUtil.toBean(setting.getSettingValue(), WechatConnectSetting.class);

        if (wechatConnectSetting == null) {
            throw new ServiceException(ResultCode.WECHAT_CONNECT_NOT_EXIST);
        }
        //寻找对应对微信小程序登录配置
        for (WechatConnectSettingItem wechatConnectSettingItem : wechatConnectSetting.getWechatConnectSettingItems()) {
            if (wechatConnectSettingItem.getClientType().equals(ClientTypeEnum.WECHAT_MP.name())) {
                return wechatConnectSettingItem;
            }
        }

        throw new ServiceException(ResultCode.WECHAT_CONNECT_NOT_EXIST);
    }


    /**
     * 解密，获取微信信息
     *
     * @param encryptedData 加密信息
     * @param sessionKey    微信sessionKey
     * @param iv            微信揭秘参数
     * @return 用户信息
     */
    public JSONObject getUserInfo(String encryptedData, String sessionKey, String iv) {

        log.info("encryptedData:{},sessionKey:{},iv:{}", encryptedData, sessionKey, iv);
        //被加密的数据
        byte[] dataByte = Base64.getDecoder().decode(encryptedData);
        //加密秘钥
        byte[] keyByte = Base64.getDecoder().decode(sessionKey);
        //偏移量
        byte[] ivByte = Base64.getDecoder().decode(iv);
        try {
            //如果密钥不足16位，那么就补足.  这个if 中的内容很重要
            int base = 16;
            if (keyByte.length % base != 0) {
                int groups = keyByte.length / base + (keyByte.length % base != 0 ? 1 : 0);
                byte[] temp = new byte[groups * base];
                Arrays.fill(temp, (byte) 0);
                System.arraycopy(keyByte, 0, temp, 0, keyByte.length);
                keyByte = temp;
            }
            //初始化
            Security.addProvider(new BouncyCastleProvider());
            Cipher cipher = Cipher.getInstance("AES/CBC/PKCS7Padding", "BC");
            SecretKeySpec spec = new SecretKeySpec(keyByte, "AES");
            AlgorithmParameters parameters = AlgorithmParameters.getInstance("AES");
            parameters.init(new IvParameterSpec(ivByte));
            //初始化
            cipher.init(Cipher.DECRYPT_MODE, spec, parameters);
            byte[] resultByte = cipher.doFinal(dataByte);
            if (null != resultByte && resultByte.length > 0) {
                String result = new String(resultByte, StandardCharsets.UTF_8);
                return JSONUtil.parseObj(result);
            }
        } catch (Exception e) {
            log.error("解密，获取微信信息错误", e);
        }
        throw new ServiceException(ResultCode.USER_CONNECT_ERROR);
    }
}